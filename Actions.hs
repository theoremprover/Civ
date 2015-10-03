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

{-
data ActionAlgebra =
	Atomic Action |
	OneOf [ActionAlgebra] |
	Sequence [ActionAlgebra] |
	Repeat ActionAlgebra
	deriving (Show,Eq,Ord)

--data Phase = StartOfGame | StartOfTurn | Trading | CityManagement | Movement | Research

--data StartOfGame_SubPhase = BuildCity | 
-}


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
	unitAttackBonus :: UnitType -> Value Strength,
	battleHandSize  :: Player -> Value Int,
	cultureCardLimit :: Player -> Value Int,
	cultureTrackTradeBonus :: Player -> Value Int,
	cultureTrackCultureBonus :: Player -> Value Int,
	unitStackLimit  :: Value Int,
	moveRange       :: Value Coor,
	cardCoins       :: Coins,
	maxCities       :: Value Int,
	buildWonderHook :: GameName -> PlayerName -> UpdateCivM (),
	enabledGovernments :: [Government],
	enabledBuildings :: [BuildingType],
	cardAbilities     :: Phase -> [(String,GameName -> PlayerName -> UpdateCivM ())],
	resourceAbilities :: Phase -> [(String,[ResourcePattern],GameName -> PlayerName -> UpdateCivM ())] }

defaultAbilities = Abilities {
	unitLevel       = \case
		Aircraft -> SetValue Nothing
		_        -> SetValue (Just UnitLevelI),
	unitAttackBonus = const $ SetValue 0,
	battleHandSize  = const $ SetValue 3,
	cultureCardLimit = const $ SetValue 2,
	cultureTrackTradeBonus = const $ SetValue 0,
	cultureTrackCultureBonus = const $ SetValue 0,
	unitStackLimit  = SetValue 2,
	moveRange       = SetValue 2,
	cardCoins       = Coins 0,
	maxCities       = SetValue 2,
	buildWonderHook = \ _ _ -> return (),
	enabledGovernments = [Despotism,Anarchy],
	enabledBuildings = [],
	cardAbilities   = const [],
	resourceAbilities = const [] }

unchangedAbilities = Abilities {
	unitLevel       = const Unchanged,
	unitAttackBonus = const Unchanged,
	battleHandSize  = const Unchanged,
	cultureCardLimit = const Unchanged,
	cultureTrackTradeBonus = const Unchanged,
	cultureTrackCultureBonus = const Unchanged,
	unitStackLimit  = Unchanged,
	moveRange       = Unchanged,
	cardCoins       = Coins 0,
	maxCities       = Unchanged,
	buildWonderHook = \ _ _ -> return (),
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
			Infantry -> ModifyValue (+1)
			_        -> ModifyValue id }
	Mongols  -> defaultAbilities
	Rome     -> defaultAbilities
	Russia   -> defaultAbilities
	Spanish  -> defaultAbilities {
		moveRange = SetValue 3 }
	Zulu     -> defaultAbilities

techAbilities tech = case tech of
	Pottery              -> unchangedAbilities
	Writing              -> unchangedAbilities
	CodeOfLaws           -> unchangedAbilities {
		enabledGovernments = [Republic],
		enabledBuildings   = [TradePost],
		cardAbilities      = cardAbility [Research] "Code Of Laws: Add Coin" addCoinAfterWonBattle_CodeOfLaws $ const [] }
	Currency             -> unchangedAbilities {
		enabledBuildings   = [Market],
		resourceAbilities  = resourceAbility [CityManagement] "Currency: Gain 3 Culture" [One Incense] (addCulture 3) $ const [] }
	Metalworking         -> unchangedAbilities
	Masonry              -> unchangedAbilities
	Agriculture          -> unchangedAbilities
	HorsebackRiding      -> unchangedAbilities
	AnimalHusbandry      -> unchangedAbilities
	Philosophy           -> unchangedAbilities
	Navigation           -> unchangedAbilities
	Navy                 -> unchangedAbilities
	PublicAdministration -> unchangedAbilities
	Mysticism            -> unchangedAbilities
	MonarchyTech         -> unchangedAbilities {
		enabledGovernments = [Monarchy] }
	DemocracyTech        -> unchangedAbilities {
		unitLevel = setUnitLevel [Infantry] UnitLevelII,
		enabledGovernments = [Democracy],
 		cardAbilities      = cardAbility [CityManagement] "Democracy: Add Coin" addCoin_Democracy $ const [] }
	Chivalry             -> unchangedAbilities {
		unitLevel = setUnitLevel [Cavalry] UnitLevelII,
		enabledGovernments = [Feudalism],
		resourceAbilities  = resourceAbility [CityManagement] "Metal Casting: Gain 5 Culture" [One Incense] (addCulture 5) $ const [] }
	Mathematics          -> unchangedAbilities {
		unitLevel = setUnitLevel [Artillery] UnitLevelII }
	Logistics            -> unchangedAbilities {
		unitLevel = setUnitLevel [Infantry,Cavalry,Artillery] UnitLevelII }
	PrintingPress        -> unchangedAbilities
	Sailing              -> unchangedAbilities
	Construction         -> unchangedAbilities {
		enabledBuildings   = [Forge],
		resourceAbilities  = resourceAbility [CityManagement] "Construction: +5 Hammers" [One Wheat] (plusHammers 5) $ const [] }
	Engineering          -> unchangedAbilities
	Irrigation           -> unchangedAbilities
	Bureaucracy          -> unchangedAbilities {
		cardCoins          = Coins 1,
		cardAbilities      = cardAbility [Research] "Bureaucracy: Switch Policy" switchPolicy_Bureaucracy $ const [] }
	Theology             -> unchangedAbilities {
		enabledGovernments = [Fundamentalism] }
	CommunismTech        -> unchangedAbilities {
		enabledGovernments = [Communism],
		cardAbilities      = cardAbility [Movement] "Communism: Lock Square" lockSquare_CommunismTech $ const [] }
	Gunpowder            -> unchangedAbilities {
		unitLevel          = setUnitLevel [Infantry] UnitLevelIII }
	Railroad             -> unchangedAbilities {
		unitLevel          = setUnitLevel [Cavalry] UnitLevelIII }
	MetalCasting         -> unchangedAbilities {
		unitLevel          = setUnitLevel [Artillery] UnitLevelIII,
		resourceAbilities  = resourceAbility [CityManagement] "Metal Casting: Gain 7 Culture" [One Incense] (addCulture 7) $ const [] }
	Ecology              -> unchangedAbilities {
		cultureTrackTradeBonus = modifyValuePerNCoins 3,
		resourceAbilities  = resourceAbility [StartOfTurn] "Ecology: Change Terrain" [One Wheat] (changeTerrain_Ecology) $ const [] }
	Biology              -> unchangedAbilities {
		unitStackLimit     = SetValue 5 }
	SteamEngine          -> unchangedAbilities
	Banking              -> unchangedAbilities {
		resourceAbilities  = resourceAbility [CityManagement] "Banking: +7 Hammers" [One Wheat] (plusHammers 7) $ const [],
		enabledBuildings   = [Bank] }
	MilitaryScience      -> unchangedAbilities
	Education            -> unchangedAbilities {
		buildWonderHook = addCoinToCard Education,
		resourceAbilities  = resourceAbility [CityManagement] "Education: Learn Tech" [One Wheat,One Incense,One Iron,One Linen] learnTech_Education $ const [] }
	Computers            -> unchangedAbilities {
		cardCoins          = Coins 1,
		battleHandSize     = modifyValuePerNCoins 5,
		cultureCardLimit   = modifyValuePerNCoins 5 }
	MassMedia            -> unchangedAbilities
	Ballistics           -> unchangedAbilities {
		unitLevel          = setUnitLevel [Artillery] UnitLevelStar,
		resourceAbilities  = resourceAbility [Battle] "Ballistics: Deal 6 Damage" [One Iron] (dealDamage 6) $ const [] }
	ReplacementParts     -> unchangedAbilities {
		unitLevel          = setUnitLevel [Infantry] UnitLevelStar }
	Flight               -> unchangedAbilities {
		unitLevel          = setUnitLevel [Aircraft] UnitLevelStar }
	Plastics             -> unchangedAbilities
	CombustionEngine     -> unchangedAbilities {
		unitLevel          = setUnitLevel [Cavalry] UnitLevelStar,
		cardAbilities      =
			cardAbility [Movement] "Combustion Engine: Destroy Building" destroyBuilding_CombustionEngine $
			cardAbility [Battle] "Combustion Engine: Destroy Walls" destroyWalls_CombustionEngine $ const [] }
	AtomicTheory         -> unchangedAbilities {
		resourceAbilities  =
			resourceAbility [CityManagement] "Atomic Theory: Additional City Actions" [One Atom] additionalCityActions_AtomicTheory $
			resourceAbility [Movement] "Atomic Theory: Nuke City" [One Atom] nukeCity_AtomicTheory $ const [] }
	SpaceFlight          -> unchangedAbilities

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

plusHammers hammers gamename playername = error "Not implemented yet"

switchPolicy_Bureaucracy gamename playername = error "Not implemented yet"

addCoinAfterWonBattle_CodeOfLaws gamename playername = error "Not implemented yet"

destroyBuilding_CombustionEngine gamename playername = error "Not implemented yet"

destroyWalls_CombustionEngine gamename playername = error "Not implemented yet"

lockSquare_CommunismTech gamename playername = error "Not implemented yet"

addCoin_Democracy gamename playername = do
	addTrade (-6) gamename playername
	addCoinToCard Democracy gamename playername

addCoinToCard tech gamename playername = error "Not implemented yet"

changeTerrain_Ecology gamename playername = error "Not implemented yet"

learnTech_Education gamename playername = error "Not implemented yet"

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
