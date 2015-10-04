{-# LANGUAGE TypeFamilies,Rank2Types,LambdaCase #-}

module Actions where

import Import(App,getYesod,Handler,MonadHandler,appCivAcid,HandlerSite)
import Prelude

import Control.Lens hiding (Action)
import Data.Acid
import Data.Acid.Advanced

import Model
import Acidic
import Lenses
import Polls


queryCivLensH :: (MonadHandler m, HandlerSite m ~ App) => Traversal' CivState a -> m (Maybe a)
queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ preview lens civstate


data Value a = ModifyValue (a -> a) | Unchanged | SetValue a
instance (Ord a) => Ord (Value a) where
	ModifyValue _ <= ModifyValue _ = True
	ModifyValue f <= _ = True
	_ <= ModifyValue g = False
	SetValue a <= SetValue b = a<=b
	SetValue a <= _ = False
	_ <= SetValue b = True
	Unchanged <= Unchanged = True

instance (Show a) => Show (Value a) where
	show (ModifyValue f) = "ModifyValue <fn>"
	show Unchanged = "Unchanged"
	show (SetValue a) = "SetValue " ++ show a
instance (Eq a) => Eq (Value a) where
	ModifyValue _ == _ = False
	_ == ModifyValue _ = False
	Unchanged == Unchanged = True
	SetValue a == SetValue b = a==b
	_ == _ = False

data ResourcePattern = One Resource | AnyResource
	deriving (Show,Eq)

data Abilities = Abilities {
	unitLevel       :: UnitType -> Value (Maybe UnitLevel),
	unitAttackBonus :: UnitType -> Strength,
	battleHandSize  :: Player -> Value Int,
	cultureCardLimit :: Player -> Value Int,
	cultureTrackTradeBonus :: Player -> Value Int,
	cultureTrackCultureBonus :: Player -> Value Int,
	unitStackLimit  :: Value Int,
	moveRange       :: Value Coor,
	movementType    :: MovementType,
	cardCoins       :: Coins,
	maxCities       :: Value Int,
	productionBonus :: Player -> Value Int,
	buildWonderHook :: GameName -> PlayerName -> UpdateCivM (),
	drawCultureHook :: GameName -> PlayerName -> UpdateCivM (),
	buildArmyHook   :: GameName -> PlayerName -> UpdateCivM (),
	getTechHook     :: GameName -> PlayerName -> UpdateCivM (),
	enabledGovernments :: [Government],
	enabledBuildings :: [BuildingType],
	cardAbilities     :: Phase -> [(String,GameName -> PlayerName -> UpdateCivM ())],
	resourceAbilities :: Phase -> [(String,[ResourcePattern],GameName -> PlayerName -> UpdateCivM ())] }

defaultAbilities = Abilities {
	unitLevel       = \case
		Aircraft -> SetValue Nothing
		_        -> SetValue (Just UnitLevelI),
	unitAttackBonus = const 0,
	battleHandSize  = const $ SetValue 3,
	cultureCardLimit = const $ SetValue 2,
	cultureTrackTradeBonus = const $ SetValue 0,
	cultureTrackCultureBonus = const $ SetValue 0,
	unitStackLimit  = SetValue 2,
	moveRange       = SetValue 2,
	movementType    = Land,
	cardCoins       = Coins 0,
	maxCities       = SetValue 2,
	productionBonus = const $ SetValue 0,
	buildWonderHook = \ _ _ -> return (),
	drawCultureHook = \ _ _ -> return (),
	buildArmyHook   = \ _ _ -> return (),
	getTechHook     = \ _ _ -> return (),
	enabledGovernments = [Despotism,Anarchy],
	enabledBuildings = [],
	cardAbilities   = const [],
	resourceAbilities = const [] }

unchangedAbilities = Abilities {
	unitLevel       = const Unchanged,
	unitAttackBonus = const 0,
	battleHandSize  = const Unchanged,
	cultureCardLimit = const Unchanged,
	cultureTrackTradeBonus = const Unchanged,
	cultureTrackCultureBonus = const Unchanged,
	unitStackLimit  = Unchanged,
	moveRange       = Unchanged,
	movementType    = Land,
	cardCoins       = Coins 0,
	maxCities       = Unchanged,
	productionBonus = const Unchanged,
	buildWonderHook = \ _ _ -> return (),
	drawCultureHook = \ _ _ -> return (),
	buildArmyHook   = \ _ _ -> return (),
	getTechHook     = \ _ _ -> return (),
	enabledGovernments = [],
	enabledBuildings = [],
	cardAbilities   = const [],
	resourceAbilities = const [] }

civAbilities civ = case civ of
	America  -> defaultAbilities
	Arabs    -> defaultAbilities
	Aztecs   -> defaultAbilities
	China    -> defaultAbilities
	Egypt    -> defaultAbilities
	English  -> defaultAbilities
	French   -> defaultAbilities
	Germany  -> defaultAbilities
	Greeks   -> defaultAbilities
	Indians  -> defaultAbilities
	Japanese -> defaultAbilities {
		unitAttackBonus = \case
			Infantry -> 1
			_        -> 0 }
	Mongols  -> defaultAbilities
	Rome     -> defaultAbilities
	Russia   -> defaultAbilities
	Spanish  -> defaultAbilities {
		moveRange = SetValue 3 }
	Zulu     -> defaultAbilities

techAbilities tech = case tech of
	Pottery              -> unchangedAbilities {
		enabledBuildings   = [Granary],
		cultureCardLimit   = const $ ModifyValue (+1),
		resourceAbilities  = resourceAbility [CityManagement] "Pottery: Gain Coin" [AnyResource,AnyResource] (addCoinToTech Pottery) $ const [] }
	Writing              -> unchangedAbilities
	CodeOfLaws           -> unchangedAbilities {
		enabledGovernments = [Republic],
		enabledBuildings   = [TradePost],
		cardAbilities      = cardAbility [Research] "Code Of Laws: Add Coin" addCoinAfterWonBattle_CodeOfLaws $ const [] }
	Currency             -> unchangedAbilities {
		enabledBuildings   = [Market],
		resourceAbilities  = resourceAbility [CityManagement] "Currency: Gain 3 Culture" [One Incense] (addCulture 3) $ const [] }
	Metalworking         -> unchangedAbilities {
		enabledBuildings   = [Barracks],
		resourceAbilities  = resourceAbility [Battle] "Metalworking: Increase Attack" [One Iron] increaseAttack_Metalworking $ const [] }
	Masonry              -> unchangedAbilities {
		unitStackLimit     = SetValue 3,
		cardAbilities      = cardAbility [CityManagement] "Masonry: Build City Walls" buildCityWalls_Masonry $ const [] }		
	Agriculture          -> unchangedAbilities {
		getTechHook        = growIntoMetropolisHook_Agriculture,
		resourceAbilities  = resourceAbility [CityManagement] "Agriculture: +3 Hammers" [One Wheat] (plusHammers 3) $ const [] }
	HorsebackRiding      -> unchangedAbilities {
		moveRange          = SetValue 3,
		resourceAbilities  = resourceAbility [Trading] "Horseback Riding: Get Trade" [One Linen] getTrade_HoresebackRiding $ const [] }
	AnimalHusbandry      -> unchangedAbilities {
		cardAbilities      = cardAbility [Battle] "Animal Husbandry: Heal 3 Damage" (healDamage 3) $ const [] }
	Philosophy           -> unchangedAbilities {
		enabledBuildings   = [Temple],
		resourceAbilities  = resourceAbility [CityManagement] "Philosophy: Get Great Person" [AnyResource,AnyResource,AnyResource] getGreatPerson $ const [] }
	Navigation           -> unchangedAbilities {
		enabledBuildings   = [Harbour],
		movementType       = CrossWater }
	Navy                 -> unchangedAbilities {
		enabledBuildings   = [Shipyard],
		buildArmyHook      = armyToShipyardOutskirtsHook_Navy }
	PublicAdministration -> unchangedAbilities {
		cardCoins          = Coins 1,
		cultureCardLimit   = const $ ModifyValue (+1),
		resourceAbilities  = resourceAbility allPhases "Public Administration: Cancel Culture Event" [One Spy] cancelCultureEvent $ const [] }
	Mysticism            -> unchangedAbilities {
		drawCultureHook    = drawAnotherCultureCardHook_Mysticism,
		resourceAbilities  = resourceAbility [CityManagement] "Mysticism: Force Discard Coin" [One Spy] forceDiscardCoin_Mysticism $ const [] }
	MonarchyTech         -> unchangedAbilities {
		enabledGovernments = [Monarchy],
		resourceAbilities  = resourceAbility [CityManagement] "Monarchy: Destroy Wonder/Unit" [One Linen] destroyWonderOrUnit_MonarchyTech $ const [] }
	DemocracyTech        -> unchangedAbilities {
		unitLevel = setUnitLevel [Infantry] UnitLevelII,
		enabledGovernments = [Democracy],
 		cardAbilities      = cardAbility [CityManagement] "Democracy: Add Coin" addCoin_Democracy $ const [] }
	Chivalry             -> unchangedAbilities {
		unitLevel = setUnitLevel [Cavalry] UnitLevelII,
		enabledGovernments = [Feudalism],
		resourceAbilities  = resourceAbility [CityManagement] "Metal Casting: Gain 5 Culture" [One Incense] (addCulture 5) $ const [] }
	Mathematics          -> unchangedAbilities {
		unitLevel = setUnitLevel [Artillery] UnitLevelII,
		resourceAbilities  = resourceAbility [Battle] "Mathematics: Deal 3 Damage" [One Iron] (dealDamage 3) $ const [] }
	Logistics            -> unchangedAbilities {
		unitLevel = setUnitLevel [Infantry,Cavalry,Artillery] UnitLevelII }
	PrintingPress        -> unchangedAbilities {
		unitStackLimit     = SetValue 4,
		enabledBuildings   = [University],
 		cardAbilities      = cardAbility [CityManagement] "Printing Press: Add Coin" addCoin_PrintingPress $ const [] }
	Sailing              -> unchangedAbilities {
		moveRange          = SetValue 4,
		movementType       = StayInWater }
	Construction         -> unchangedAbilities {
		enabledBuildings   = [Forge],
		resourceAbilities  = resourceAbility [CityManagement] "Construction: +5 Hammers" [One Wheat] (plusHammers 5) $ const [] }
	Engineering          -> unchangedAbilities {
		enabledBuildings   = [Aquaeduct],
 		cardAbilities      = cardAbility [CityManagement] "Engineering: Split Production" splitProduction_Engineering $ const [] }
	Irrigation           -> unchangedAbilities {
		maxCities          = SetValue 3 }
	Bureaucracy          -> unchangedAbilities {
		cardCoins          = Coins 1,
		cardAbilities      = cardAbility [Research] "Bureaucracy: Switch Policy" switchPolicy_Bureaucracy $ const [] }
	Theology             -> unchangedAbilities {
		enabledGovernments = [Fundamentalism],
		enabledBuildings   = [Cathedral],
		cultureCardLimit   = const $ ModifyValue (+1) }
	CommunismTech        -> unchangedAbilities {
		enabledGovernments = [Communism],
		cardAbilities      = cardAbility [Movement] "Communism: Lock Square" lockSquare_CommunismTech $ const [] }
	Gunpowder            -> unchangedAbilities {
		unitLevel          = setUnitLevel [Infantry] UnitLevelIII,
		resourceAbilities  = resourceAbility [CityManagement] "Gunpowder: Destroy Wonder/Buliding" [AnyResource,AnyResource] destroyWonderBuilding_Gunpowder $ const [] }
	Railroad             -> unchangedAbilities {
		enabledBuildings   = [IronMine],
		cardCoins          = Coins 1,
		unitLevel          = setUnitLevel [Cavalry] UnitLevelIII }
	MetalCasting         -> unchangedAbilities {
		unitLevel          = setUnitLevel [Artillery] UnitLevelIII,
		resourceAbilities  = resourceAbility [CityManagement] "Metal Casting: Gain 7 Culture" [One Incense] (addCulture 7) $ const [] }
	Ecology              -> unchangedAbilities {
		cultureTrackTradeBonus = modifyValuePerNCoins 3,
		resourceAbilities  = resourceAbility [StartOfTurn] "Ecology: Change Terrain" [One Wheat] (changeTerrain_Ecology) $ const [] }
	Biology              -> unchangedAbilities {
		unitStackLimit     = SetValue 5,
		cardAbilities      = cardAbility [Battle] "Animal Husbandry: Heal All Damage" healAllDamage $ const [] }
	SteamEngine          -> unchangedAbilities {
		moveRange          = SetValue 5,
		movementType       = StayInWater,
		resourceAbilities  = resourceAbility [CityManagement] "Steam Engine: Move Figures" [One Linen] moveFigures_SteamEngine $ const [] }
	Banking              -> unchangedAbilities {
		resourceAbilities  = resourceAbility [CityManagement] "Banking: +7 Hammers" [One Wheat] (plusHammers 7) $ const [],
		enabledBuildings   = [Bank] }
	MilitaryScience      -> unchangedAbilities {
		enabledBuildings   = [Academy],
		productionBonus    = modifyValuePerNCoins 3 }
	Education            -> unchangedAbilities {
		buildWonderHook = addCoinToCard Education,
		resourceAbilities  = resourceAbility [CityManagement] "Education: Learn Tech" [One Wheat,One Incense,One Iron,One Linen] learnTech_Education $ const [] }
	Computers            -> unchangedAbilities {
		cardCoins          = Coins 1,
		battleHandSize     = modifyValuePerNCoins 5,
		cultureCardLimit   = modifyValuePerNCoins 5 }
	MassMedia            -> unchangedAbilities {
		cardAbilities      = cardAbility allPhases "Mass Media: Immune Culture Events" cancelCultureEventCancel_MassMedia $ const [],
		resourceAbilities  = resourceAbility allPhases "Mass Media: Cancel Resource Ability" [One Spy] cancelResourceAbility_MassMedia $ const [] }
	Ballistics           -> unchangedAbilities {
		unitLevel          = setUnitLevel [Artillery] UnitLevelStar,
		resourceAbilities  = resourceAbility [Battle] "Ballistics: Deal 6 Damage" [One Iron] (dealDamage 6) $ const [] }
	ReplacementParts     -> unchangedAbilities {
		unitStackLimit     = SetValue 6,
		unitLevel          = setUnitLevel [Infantry] UnitLevelStar }
	Flight               -> unchangedAbilities {
		unitLevel          = setUnitLevel [Aircraft] UnitLevelStar,
		moveRange          = SetValue 6,
		movementType       = Air }
	Plastics             -> unchangedAbilities {
		cardAbilities      = cardAbility [StartOfTurn] "Plastics: Build Unit/Figure/Building" buildUnitFigureBuilding_Plastics $ const [],
		resourceAbilities  = resourceAbility [CityManagement] "Plastics: +10 Hammers" [One Wheat] (plusHammers 10) $ const [] }
	CombustionEngine     -> unchangedAbilities {
		unitLevel          = setUnitLevel [Cavalry] UnitLevelStar,
		cardAbilities      =
			cardAbility [Movement] "Combustion Engine: Destroy Building" destroyBuilding_CombustionEngine $
			cardAbility [Battle] "Combustion Engine: Destroy Walls" destroyWalls_CombustionEngine $ const [] }
	AtomicTheory         -> unchangedAbilities {
		resourceAbilities  =
			resourceAbility [CityManagement] "Atomic Theory: Additional City Actions" [One Atom] additionalCityActions_AtomicTheory $
			resourceAbility [Movement] "Atomic Theory: Nuke City" [One Atom] nukeCity_AtomicTheory $ const [] }
	SpaceFlight          -> unchangedAbilities {
		getTechHook        = victory }

	where

	setUnitLevel unittypes unitlevel ut | ut `elem` unittypes = SetValue (Just unitlevel)
	setUnitLevel _ _ _ = Unchanged

	resourceAbility phases name respats action f phase | phase `elem` phases = [(name,respats,action)]
	resourceAbility _ _ _ _ f phase = f phase

	cardAbility phases name action f phase | phase `elem` phases = [(name,action)]
	cardAbility _ _ _ f phase = f phase

modifyValuePerNCoins n player = ModifyValue (+(mod (numberOfCoins player) n))

additionalCityActions_AtomicTheory gamename playername = error "Not implemented yet"

nukeCity_AtomicTheory gamename playername = error "Not implemented yet"

dealDamage damage gamename playername = error "Not implemented yet"

healDamage damage gamename playername = error "Not implemented yet"
healAllDamage gamename playername = error "Not implemented yet"

plusHammers hammers gamename playername = error "Not implemented yet"

switchPolicy_Bureaucracy gamename playername = error "Not implemented yet"

addCoinAfterWonBattle_CodeOfLaws gamename playername = error "Not implemented yet"

destroyBuilding_CombustionEngine gamename playername = error "Not implemented yet"

destroyWalls_CombustionEngine gamename playername = error "Not implemented yet"

lockSquare_CommunismTech gamename playername = error "Not implemented yet"

destroyWonderBuilding_Gunpowder gamename playername = error "Not implemented yet"

getTrade_HoresebackRiding gamename playername = error "Not implemented yet"

cancelCultureEvent gamename playername = error "Not implemented yet"

addCoin_Democracy gamename playername = do
	addTrade (-6) gamename playername
	addCoinToCard Democracy gamename playername

addCoinToCard tech gamename playername = error "Not implemented yet"

addCoin_PrintingPress gamename playername = do
	addCulture (-5) gamename playername
	addCoinToCard PrintingPress gamename playername

changeTerrain_Ecology gamename playername = error "Not implemented yet"

learnTech_Education gamename playername = error "Not implemented yet"

growIntoMetropolisHook_Agriculture gamename playername = error "Not implemented yet"

buildCityWalls_Masonry gamename playername = error "Not implemented yet"

cancelCultureEventCancel_MassMedia gamename playername = error "Not implemented yet"

cancelResourceAbility_MassMedia gamename playername = error "Not implemented yet"

increaseAttack_Metalworking gamename playername = error "Not implemented yet"

destroyWonderOrUnit_MonarchyTech gamename playername = error "Not implemented yet"

drawAnotherCultureCardHook_Mysticism gamename playername = error "Not implemented yet"

forceDiscardCoin_Mysticism gamename playername = error "Not implemented yet"

armyToShipyardOutskirtsHook_Navy gamename playername = error "Not implemented yet"

buildUnitFigureBuilding_Plastics gamename playername = error "Not implemented yet"

splitProduction_Engineering gamename playername = error "Not implemented yet"

moveFigures_SteamEngine gamename playername = error "Not implemented yet"

victory gamename playername = error "Not implemented yet"

valueAbilities :: (Ord a) => [Value a] -> a
valueAbilities values = foldl (\ x f -> f x) a modvalues
	where
	SetValue a = maximum values
	modvalues = map ismod values
	ismod (ModifyValue f) = f
	ismod _ = id

unitLevelAbilities :: Player -> UnitType -> Maybe UnitLevel
unitLevelAbilities player@(Player{..}) unittype = valueAbilities $
	map (\ f -> f unittype) $ map unitLevel $ civAbilities _playerCiv : map (techAbilities._techCardTechId) _playerTechs

numberOfCoins :: Player -> Int
numberOfCoins player@(Player{..}) = coinsCoins _playerCoins + sum (map (coinsCoins.cardCoins.techAbilities._techCardTechId) _playerTechs)
	-- TODO: Add Coins auf dem Battlefield

moveGen :: GameName -> Game -> Maybe PlayerName -> [Action]
moveGen _ _ Nothing = []
moveGen gamename game@(Game{..}) (Just my_playername) = do
	let
		(playername_turn,player_turn@(Player{..})) = nthAssocList _gamePlayersTurn _gamePlayers
		in case my_playername == playername_turn of
			False -> []
			True -> case _gamePhase of
				StartOfGame -> []
				BuildingFirstCity -> map (BuildFirstCity gamename playername_turn) _playerFirstCityCoors
				GettingFirstTrade -> [GetFirstTrade gamename playername_turn]
				_ -> []

{-

Arten von Fähigkeiten:
1. Zusätzliche Aktion (z.B. StartOfTurn: Kultureinkommen, Forschung; CityManagement: Zusätzl. Aktion)
2. Modifizierend (Bewegungsweite,Einkommen,Figurenlimit,Mögliche Regierungsform,...)

StartOfTurn:
- Regierungsform ändern
- Persönlichkeit einsetzen/runternehmen

Trading:
- Handel bekommen (abhängig von Wundern, Karten etc.)

CityManagement:
- Für jede Stadt: Produktion | Kultur machen | Resource ernten

Movement:
- Bewegen (bewegungsweite hängt von Techs/Persönlichkeiten ab)

Research:
- Forschung

ALLE:
- Resourcefähigkeit spielen
- Persönlichkeiten spielen
- Kulturkarte spielen

-}
