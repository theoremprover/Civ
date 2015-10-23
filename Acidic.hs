{-# LANGUAGE TypeFamilies,RankNTypes,ScopedTypeVariables,LambdaCase #-}

module Acidic where

import Import (ask,lift,App,getYesod,Handler,MonadHandler,appCivAcid,HandlerSite)
--hiding (Update,Query,array,delete,head,(++),map,zip,unlines,concatMap,filter)

import Prelude

import Data.Acid
import Data.Acid.Advanced
import Control.Monad.Error (throwError,runErrorT,ErrorT)
import Data.Maybe
import Data.List
import Control.Monad
import Control.Lens hiding (Action)
import qualified Data.Map as Map
import Control.Monad.State (modify,get,gets,MonadState)
import Data.List (delete)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Monoid

import Data.Array.IArray (array,(//),assocs)
import Data.Ix

import Logic
import Lenses
import TokenStack
import Model
import Polls
import ModelVersion


data Value a = ModifyValue (a -> a) | Unchanged | SetValue a
instance (Ord a) => Ord (Value a) where
	ModifyValue _ <= ModifyValue _ = True
	ModifyValue f <= _ = True
	_ <= ModifyValue g = False
	SetValue a <= SetValue b = a<=b
	SetValue a <= _ = False
	_ <= SetValue b = False
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

type HookM a = GameName -> PlayerName -> UpdateCivM a

noopHookM = constHookM ()
constHookM a _ _ = return a

data Abilities = Abilities {
	unitLevel                :: UnitType -> Value (Maybe UnitLevel),
	unitAttackBonus          :: UnitType -> Strength,
	lootBonus                :: Value Int,
	battleStrengthBonus      :: Value Strength,
	battleHandSize           :: Player -> Value Int,
	cultureCardLimit         :: Player -> Value Int,
	cultureTrackTradeBonus   :: Player -> Value Int,
	cultureTrackCultureBonus :: Player -> Value Int,
	researchCostBonus   :: Value Trade,
	unitStackLimit      :: Value Int,
	moveRange           :: Value Coor,
	movementType        :: MovementType,
	cardCoins           :: Coins,
	threeTradeHammers   :: Value Int,
	maxCities           :: Value Int,
	productionBonus     :: Player -> Value Int,
	buildWonderHook     :: HookM (),
	drawCultureHook     :: HookM (),
	buildArmyHook       :: HookM (),
	getThisHook         :: HookM (),
	getTechHook         :: Tech -> HookM (),
	startOfGameHook     :: HookM (),
	spendResourceHook   :: HookM (),
	investCoinHook      :: HookM (),
	afterBattleHook     :: [UnitCard] -> [UnitCard] -> HookM (),
	getGreatPersonHook  :: HookM (),
	wonBattleHook       :: HookM (),
	discoverHutHook     :: HookM (),
	discoverVillageHook :: HookM (),
	conquerCityHook     :: HookM (),
	devoteToArtsBonusHook :: Coors -> HookM Culture,
	exploreTileHook     :: HookM (),
	indianResourceSpending :: Value Bool,
	enabledGovernments  :: [Government],
	enabledBuildings    :: [BuildingType],
	armiesAsScouts      :: Value Bool,
	wondersNonobsoletable :: Value Bool,
	sacrificeForTech    :: Value Bool,
	exploreHutWithoutBattle :: Value Bool,
	buildCityNextToHuts :: Value Bool,
	canBuildMetropolis  :: Value Bool,
	cardAbilities       :: Phase -> [(String,HookM ())],
	resourceAbilities   :: Phase -> [(String,[ResourcePattern],HookM ())] }

defaultAbilities = Abilities {
	unitLevel       = \case
		Aircraft -> SetValue Nothing
		_        -> SetValue (Just UnitLevelI),
	unitAttackBonus = const 0,
	lootBonus       = SetValue 0,
	battleStrengthBonus = SetValue 0,
	battleHandSize  = const $ SetValue 3,
	cultureCardLimit = const $ SetValue 2,
	cultureTrackTradeBonus = const $ SetValue 0,
	cultureTrackCultureBonus = const $ SetValue 0,
	researchCostBonus = SetValue (Trade 0),
	unitStackLimit  = SetValue 2,
	moveRange       = SetValue 2,
	movementType    = Land,
	cardCoins       = Coins 0,
	threeTradeHammers = SetValue 1,
	maxCities       = SetValue 2,
	productionBonus = const $ SetValue 0,
	buildWonderHook = noopHookM,
	drawCultureHook = noopHookM,
	buildArmyHook   = noopHookM,
	getThisHook     = noopHookM,
	getTechHook     = \ _ -> noopHookM,
	startOfGameHook = noopHookM,
	spendResourceHook  = noopHookM,
	investCoinHook     = noopHookM,
	afterBattleHook    = \ _ _ -> noopHookM,
	getGreatPersonHook = noopHookM,
	wonBattleHook      = noopHookM,
	discoverHutHook     = noopHookM,
	discoverVillageHook = noopHookM,
	conquerCityHook     = noopHookM,
	devoteToArtsBonusHook  = \ _ -> constHookM (Culture 0),
	exploreTileHook    = noopHookM,
	indianResourceSpending = SetValue False,
	enabledGovernments  = [Despotism,Anarchy],
	enabledBuildings    = [],
	armiesAsScouts      = SetValue False,
	wondersNonobsoletable = SetValue False,
	sacrificeForTech    = SetValue False,
	exploreHutWithoutBattle = SetValue False,
	buildCityNextToHuts = SetValue False,
	canBuildMetropolis  = SetValue False,
	cardAbilities       = const [],
	resourceAbilities   = const [] }

unchangedAbilities = Abilities {
	unitLevel       = const Unchanged,
	unitAttackBonus = const 0,
	lootBonus       = Unchanged,
	battleStrengthBonus = Unchanged,
	battleHandSize  = const Unchanged,
	cultureCardLimit = const Unchanged,
	cultureTrackTradeBonus = const Unchanged,
	cultureTrackCultureBonus = const Unchanged,
	researchCostBonus = Unchanged,
	unitStackLimit  = Unchanged,
	moveRange       = Unchanged,
	movementType    = Land,
	cardCoins       = Coins 0,
	threeTradeHammers = Unchanged,
	maxCities       = Unchanged,
	productionBonus = const Unchanged,
	buildWonderHook = noopHookM,
	drawCultureHook = noopHookM,
	buildArmyHook   = noopHookM,
	getThisHook     = noopHookM,
	getTechHook     = \ _ -> noopHookM,
	startOfGameHook = noopHookM,
	spendResourceHook  = noopHookM,
	investCoinHook     = noopHookM,
	afterBattleHook    = \ _ _ -> noopHookM,
	getGreatPersonHook = noopHookM,
	wonBattleHook      = noopHookM,
	discoverHutHook     = noopHookM,
	discoverVillageHook = noopHookM,
	conquerCityHook     = noopHookM,
	devoteToArtsBonusHook  = \ _ -> constHookM (Culture 0),
	exploreTileHook    = noopHookM,
	indianResourceSpending = Unchanged,
	enabledGovernments  = [],
	enabledBuildings    = [],
	armiesAsScouts      = SetValue False,
	wondersNonobsoletable = Unchanged,
	sacrificeForTech    = Unchanged,
	exploreHutWithoutBattle = Unchanged,
	buildCityNextToHuts = Unchanged,
	canBuildMetropolis  = Unchanged,
	cardAbilities       = const [],
	resourceAbilities   = const [] }

civAbilities civ = case civ of
	America  -> defaultAbilities {
		startOfGameHook   = getGreatPerson,
		threeTradeHammers = SetValue 2 }
	Arabs    -> defaultAbilities {
		startOfGameHook   = \ gn pn -> forM_ [Iron,Linen,Incense,Wheat] (getResource gn pn),
		spendResourceHook = addCulture 1,
		investCoinHook    = drawCultureCard }
	Aztecs   -> defaultAbilities {
		afterBattleHook = \ ownunitskilled enemyunitskilled ->
			addCulture (Culture $ length ownunitskilled + length enemyunitskilled),
		getGreatPersonHook = build2UnitsHook_Aztecs,
		wonBattleHook      = addTrade 3 }
	China    -> defaultAbilities {
		startOfGameHook = buildCapitalWallsHook_China,
		discoverHutHook = addCulture 3,
		discoverVillageHook = addCulture 3,
		afterBattleHook = \ ownunitskilled enemyunitskilled -> resurrectOneUnitHook_China ownunitskilled }
	Egypt    -> defaultAbilities {
		wondersNonobsoletable = SetValue True,
		startOfGameHook = startBuildWonderHook_Egypt,
		cardAbilities = cardAbility [CityManagement] "Egypt: Free Building" freeBuilding_Egypt $ const [] }
	English  -> defaultAbilities {
		movementType = CrossWater,
		armiesAsScouts = SetValue True }
	French   -> defaultAbilities {
		battleStrengthBonus = ModifyValue (+2),
		startOfGameHook     = extraStartPolicyHook_French }
	Germany  -> defaultAbilities {
		startOfGameHook = \ gn pn -> forM_ [Infantry,Infantry] $ drawUnit gn pn,
		getTechHook     = getTechHook_Germany }
	Greeks   -> defaultAbilities {
		getTechHook        = getTechHook_Greeks,
		getGreatPersonHook = drawExtraPersonHook_Greeks }
	Indians  -> defaultAbilities {
		indianResourceSpending = SetValue True,
		devoteToArtsBonusHook  = extraCultureDevote_Indians }
	Japanese -> defaultAbilities {
		researchCostBonus = SetValue (Trade 3),
		unitAttackBonus = \case
			Infantry -> 1
			_        -> 0 }
	Mongols  -> defaultAbilities {
		startOfGameHook = \ gn pn -> forM_ [Cavalry,Cavalry] $ drawUnit gn pn,
		lootBonus       = ModifyValue (+1) }
	Rome     -> defaultAbilities {
		buildWonderHook     = drawCultureCard,
		discoverVillageHook = drawCultureCard,
		conquerCityHook     = drawCultureCard }
	Russia   -> defaultAbilities {
		startOfGameHook  = getFigure Flag,
		unitStackLimit   = ModifyValue (+1),
		sacrificeForTech = SetValue True }
	Spanish  -> defaultAbilities {
		startOfGameHook = getFigure Wagon,
		moveRange       = ModifyValue (+1),
		exploreTileHook = buildUnlockedBuilding_Spanish }
	Zulu     -> defaultAbilities {
		startOfGameHook         = \ gn pn -> forM_ [Artillery,Artillery] $ drawUnit gn pn,
		exploreHutWithoutBattle = SetValue True,
		buildCityNextToHuts     = SetValue True }

techAbilities tech = case tech of
	Pottery              -> unchangedAbilities {
		enabledBuildings   = [Granary],
		cultureCardLimit   = const $ ModifyValue (+1),
		resourceAbilities  = resourceAbility [CityManagement] "Pottery: Gain Coin" [AnyResource,AnyResource] (addCoinToTech Pottery) $ const [] }
	Writing              -> unchangedAbilities {
		enabledBuildings   = [Library],
		resourceAbilities  = resourceAbility [CityManagement] "Writing: Cancel City Action" [One Spy] cancelCityAction_Writing $ const [] }
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
		getThisHook        = growIntoMetropolisHook_Agriculture,
		canBuildMetropolis = SetValue True }
	HorsebackRiding      -> unchangedAbilities {
		moveRange          = SetValue 3,
		resourceAbilities  = resourceAbility [Trading] "Horseback Riding: Get Trade" [One Linen] getTrade_HoresebackRiding $ const [] }
	AnimalHusbandry      -> unchangedAbilities {
		cardAbilities      = cardAbility [Battle] "Animal Husbandry: Heal 3 Damage" (healDamage 3) $ const [],
		resourceAbilities  = resourceAbility [CityManagement] "Animal Husbandry: +3 Hammers" [One Wheat] (plusHammers 3) $ const [] }
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
		buildWonderHook    = addCoinToCard Education,
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
		getThisHook        = victory TechVictory }

	where

	setUnitLevel unittypes unitlevel ut | ut `elem` unittypes = SetValue (Just unitlevel)
	setUnitLevel _ _ _ = Unchanged

	resourceAbility phases name respats action f phase | phase `elem` phases = [(name,respats,action)]
	resourceAbility _ _ _ _ f phase = f phase

cardAbility phases name action f phase | phase `elem` phases = [(name,action)]
cardAbility _ _ _ f phase = f phase

modifyValuePerNCoins n player = ModifyValue (+(mod (coinsCoins $ playerNumberOfCoins player) n))

additionalCityActions_AtomicTheory gamename playername = do
	--TODO
	return ()

nukeCity_AtomicTheory gamename playername = do
	--TODO
	return ()

dealDamage damage gamename playername = do
	--TODO
	return ()

healDamage damage gamename playername = do
	--TODO
	return ()
healAllDamage gamename playername = do
	--TODO
	return ()

plusHammers hammers gamename playername = do
	--TODO
	return ()

switchPolicy_Bureaucracy gamename playername = do
	--TODO
	return ()

addCoinAfterWonBattle_CodeOfLaws gamename playername = do
	--TODO
	return ()

destroyBuilding_CombustionEngine gamename playername = do
	--TODO
	return ()

destroyWalls_CombustionEngine gamename playername = do
	--TODO
	return ()

lockSquare_CommunismTech gamename playername = do
	--TODO
	return ()

destroyWonderBuilding_Gunpowder gamename playername = do
	--TODO
	return ()

getTrade_HoresebackRiding gamename playername = do
	--TODO
	return ()

cancelCultureEvent gamename playername = do
	--TODO
	return ()

addCoin_Democracy gamename playername = do
	addTrade (-6) gamename playername
	addCoinToCard Democracy gamename playername

addCoinToCard tech gamename playername = do
	--TODO
	return ()

addCoin_PrintingPress gamename playername = do
	addCulture (-5) gamename playername
	addCoinToCard PrintingPress gamename playername

changeTerrain_Ecology gamename playername = do
	--TODO
	return ()

learnTech_Education gamename playername = do
	--TODO
	return ()

growIntoMetropolisHook_Agriculture gamename playername = do
	--TODO
	return ()

buildCityWalls_Masonry gamename playername = do
	--TODO
	return ()

cancelCultureEventCancel_MassMedia gamename playername = do
	--TODO
	return ()

cancelResourceAbility_MassMedia gamename playername = do
	--TODO
	return ()

increaseAttack_Metalworking gamename playername = do
	--TODO
	return ()

destroyWonderOrUnit_MonarchyTech gamename playername = do
	--TODO
	return ()

drawAnotherCultureCardHook_Mysticism gamename playername = do
	--TODO
	return ()

forceDiscardCoin_Mysticism gamename playername = do
	--TODO
	return ()

armyToShipyardOutskirtsHook_Navy gamename playername = do
	--TODO
	return ()

buildUnitFigureBuilding_Plastics gamename playername = do
	--TODO
	return ()

splitProduction_Engineering gamename playername = do
	--TODO
	return ()

moveFigures_SteamEngine gamename playername = do
	--TODO
	return ()

cancelCityAction_Writing gamename playername = do
	--TODO
	return ()

build2UnitsHook_Aztecs gamename playername = do
	--TODO
	return ()

resurrectOneUnitHook_China killedunits gamename playername = do
	--TODO
	return ()

buildCapitalWallsHook_China gamename playername = do
	--TODO
	return ()

freeBuilding_Egypt gamename playername = do
	--TODO
	return ()

startBuildWonderHook_Egypt gamename playername = do
	--TODO
	return ()

extraStartPolicyHook_French gamename playername = do
	--TODO
	return ()

getTechHook_Germany gamename playername tech = do
	--TODO
	return ()

getTechHook_Greeks gamename playername tech = do
	--TODO
	return ()

drawExtraPersonHook_Greeks gamename playername = do
	--TODO
	return ()

extraCultureDevote_Indians gamename playername citycoors = do
	--TODO
	return $ Culture 0

buildUnlockedBuilding_Spanish gamename playername = do
	--TODO
	return ()

valueAbilities :: (Ord a,Show a) => [Value a] -> a
valueAbilities values = foldl (flip ($)) a modvalues
	where
	a = case maximum values of
		SetValue v -> v
		x -> error $ "valueAbilities: values=" ++ show values
	modvalues = map ismod values
	ismod (ModifyValue f) = f
	ismod _ = id

playerUnitLevel :: Player -> UnitType -> Maybe UnitLevel
playerUnitLevel player unittype = getValueAbility (\ abilities -> (unitLevel abilities) unittype) player

playerNumberOfCoins :: Player -> Coins
playerNumberOfCoins player@(Player{..}) =
	_playerCoins +
	sum (map cardCoins (playerAbilities player))
	-- TODO: z.B. Add Coins auf dem Battlefield

-- abstrahieren auf ein Argument für ability
getValueAbility :: (Ord a,Show a) => (Abilities -> Value a) -> Player -> a
getValueAbility toability player = valueAbilities $ map toability (playerAbilities player)

playerAbilities player@(Player{..}) =
	defaultAbilities : civAbilities _playerCiv :
	map (techAbilities._techCardTechId) _playerTechs


------------

getCivState :: Query CivState CivState
getCivState = Import.ask

type UpdateCivM a = ErrorT String (Update CivState) a

checkCondition :: String -> Traversal' CivState b -> (b -> Bool) -> UpdateCivM ()
checkCondition errmsg lens f = do
	mb_v <- queryCivLensM lens
	case mb_v of
		Just v | f v -> return ()
		_ -> throwError errmsg

createNewGame :: GameName -> Game -> Update CivState UpdateResult
createNewGame gamename game = runUpdateCivM $ do
	checkCondition ("Cannot create " ++ show gamename ++ ": it already exists!")
		(civGameLens gamename) isNothing
	updateCivLensM (\_-> Just $ game) $ civGameLens gamename

deleteGame :: GameName -> Update CivState UpdateResult
deleteGame gamename = runUpdateCivM $ do
	updateCivLensM (\_-> Nothing) $ civGameLens gamename

joinGame :: GameName -> PlayerName -> PlayerEmail -> Colour -> Civ -> Update CivState UpdateResult
joinGame gamename playername email colour civ = runUpdateCivM $ do
	checkCondition (show playername ++ " already exists in " ++ show gamename)
		(civPlayerLens gamename playername) isNothing
	checkCondition (show colour ++ " already taken in " ++ show gamename)
		(civGameLens gamename . _Just . gamePlayers) ((notElem colour) . (map (_playerColour.snd)) . fromAssocList)
	updateCivLensM (\_ -> Just $ makePlayer email colour civ) $
		civGameLens gamename . _Just . gamePlayers . assocListLens playername

startGame :: GameName -> Update CivState UpdateResult
startGame gamename = runUpdateCivM $ do
	checkCondition ("Cannot start " ++ show gamename ++ ", it is not in waiting state.")
		(civGameLens gamename . _Just . gameState) (==Waiting)
	updateCivLensM (const Running) $ civGameLens gamename . _Just . gameState
	createBoard gamename

	Just players <- queryCivLensM $ civPlayersLens gamename
	updateCivLensM (const $ initialResourceStack (numPlayers players)) $
		civGameLens gamename . _Just . gameResourceStack

	forM (fromAssocList players) $ \ (playername,player@(Player{..})) -> do
		forM [Artillery,Infantry,Cavalry] $ drawUnit gamename playername
		let (starttech,startgov) = civStartTechAndGov _playerCiv
		addTech gamename playername (Just TechLevelI) starttech
		setGovernment startgov gamename playername

	Just (pn0,p0) <- queryCivLensM $ civPlayerIndexLens gamename 0
	Just (pn1,p1) <- queryCivLensM $ civPlayerIndexLens gamename 1

	forM_ [HorsebackRiding,Agriculture,Metalworking,DemocracyTech] $ addTech gamename pn0 Nothing
	addCoinToTech DemocracyTech gamename pn0
	addCoinToTech DemocracyTech gamename pn0
	forM_ [Pottery,Currency,CodeOfLaws,MonarchyTech,Mathematics,Banking] $ addTech gamename pn1 Nothing
	addCoinToTech CodeOfLaws gamename pn1

	addCulture 20 gamename pn0
	addTrade 21 gamename pn0
	addCulture 31 gamename pn1
	addTrade 11 gamename pn1

	getResource gamename pn0 Linen
	getResource gamename pn0 Iron
	getHut gamename pn0 $ ResourceHut Wheat
	getHut gamename pn0 $ ThreeCulture
	getVillage gamename pn0 $ ResourceVillage Incense
	getVillage gamename pn0 FourHammers
	getArtifact gamename pn0 AttilaVillage

	getResource gamename pn1 Wheat
	getHut gamename pn1 $ ResourceHut Iron
	getVillage gamename pn1 $ ResourceVillage Incense
	getVillage gamename pn1 SixCulture

	addCoins (Coins 1) gamename pn0
	addCoins (Coins 3) gamename pn1

	drawPolicy gamename pn0 MilitaryTradition
	drawPolicy gamename pn0 Rationalism
	drawPolicy gamename pn1 NaturalReligion
	drawPolicy gamename pn1 UrbanDevelopment
	drawPolicy gamename pn1 MilitaryTradition

	Prelude.sequence_ $ replicate 3 $ drawCultureCard gamename pn0
	Prelude.sequence_ $ replicate 4 $ drawCultureCard gamename pn1

	updateCivLensM (const BuildingFirstCity) $ civGameLens gamename . _Just . gamePhase
{-
	buildCity gamename (Coors 6 13) $ City pn0 True False False NoWalls False (Just Southward)
	buildCity gamename (Coors 6 10) $ City pn0 False False False Walls False Nothing
	buildCity gamename (Coors 2 1) $ City pn1 True False False Walls False (Just Eastward)
	buildCity gamename (Coors 2 4) $ City pn1 True False False NoWalls False (Just Eastward)
	buildCity gamename (Coors 5 1) $ City pn1 False False False NoWalls False Nothing
-}

getPlayer gamename playername = do
	Just player <- queryCivLensM $ civPlayerLens gamename playername
	return player

getGame gamename = do
	Just game <- queryCivLensM $ civGameLens gamename
	return game

getNumPlayers gamename = do
	Just players <- queryCivLensM $ civPlayersLens gamename
	return $ numPlayers players

incPlayerIndex :: GameName -> Lens' Game Int -> UpdateCivM ()
incPlayerIndex gamename playerindex = do
	numplayers <- getNumPlayers gamename
	updateCivLensM ((`mod` numplayers) . (+1)) $ civGameLens gamename . _Just . playerindex

getPlayerTurn :: GameName -> UpdateCivM PlayerName
getPlayerTurn gamename = do
	Just playerindex <- queryCivLensM $ civGameLens gamename . _Just . gamePlayersTurn
	Just (playername,_) <- queryCivLensM $ civPlayerIndexLens gamename playerindex
	return playername

finishPlayerPhase gamename = do
	incPlayerIndex gamename gamePlayersTurn
	Just (game@Game{..}) <- queryCivLensM $ civGameLens gamename . _Just
	when (_gamePlayersTurn == _gameStartPlayer) $ do
		updateCivLensM nextPhase $ civGameLens gamename . _Just . gamePhase
		Just phase <- queryCivLensM $ civGameLens gamename . _Just . gamePhase
		when (phase==StartOfTurn) $ do
			incPlayerIndex gamename gamePlayersTurn
			incPlayerIndex gamename gameStartPlayer

allowSecondMove secondmove move = case (move,secondmove) of
	(Move _ (GetTradeTarget _),Move _ (GetTradeTarget _)) -> False
	(Move _ (BuildFirstCityTarget _ _),Move _ (BuildFirstCityTarget _ _)) -> False
	(Move (FigureSource _ fig1) (SquareTarget _),Move (FigureSource _ fig2) (SquareTarget _)) | fig1==fig2 -> False
	(Move (CityProductionSource coors1 _) (SquareTarget _),Move (CityProductionSource coors2 _) (SquareTarget _)) | coors1==coors2 -> False
	_ -> True

getCity gamename coors = do
	Just city <- queryCivLensM $ civSquareLens gamename coors . squareTokenMarker . _Just . cityMarker
	return city

outskirtsOfCity :: GameName -> Coors -> UpdateCivM [Coors]
outskirtsOfCity gamename coors = do
	city <- getCity gamename coors
	return $ outskirtsOf $ coors : case _cityMetropolisOrientation city of
		Nothing -> []
		Just ori -> [addCoorsOri coors ori]

figuresOfPlayerOnSquare playername Square{..} =
	concatMap (\ (fig,pns) -> map (const fig) $ filter (==playername) pns) $
		tokenStackToList _squareFigures

buildFigure gamename playername figure coors = do
	Just (Square{..}) <- getSquare gamename coors
	Just () <- takeFromStackM (civPlayerLens gamename playername . _Just . playerFigures) figure
	putOnStackM (civSquareLens gamename coors . squareFigures) figure playername

gameAction :: GameName -> PlayerName -> Move -> Update CivState UpdateResult
gameAction gamename playername move = runUpdateCivM $ do
	moves <- moveGenM gamename playername
	case move `elem` moves of
		False -> error $ show playername ++ " requested " ++ show move ++ " which is not in 'moves'!"
		True -> do
			doMove gamename playername move
			checkMovesLeft gamename
	return ()

forAllCities :: GameName -> PlayerName -> ((Coors,City) -> UpdateCivM a) -> UpdateCivM [a]
forAllCities gamename playername action = do
	Just coorss <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerCityCoors
	forM coorss $ \ coors -> do
		city <- getCity gamename coors
		action (coors,city)

forCityOutskirts :: GameName -> PlayerName -> Coors -> ((Coors,City,Square) -> UpdateCivM a) -> UpdateCivM [a]
forCityOutskirts gamename playername coors action = do
	city <- getCity gamename coors
	outskirts <- outskirtsOfCity gamename coors
	forM outskirts $ \ outskirt_coors -> do
		Just square <- getSquare gamename outskirt_coors
		action (outskirt_coors,city,square)

forAllOutskirts :: GameName -> PlayerName -> ((Coors,City,Square) -> UpdateCivM a) -> UpdateCivM [a]
forAllOutskirts gamename playername action = do
	lss <- forAllCities gamename playername $ \ (coors,_) -> do
		forCityOutskirts gamename playername coors action
	return $ concat lss

cityIncome :: GameName -> PlayerName -> Coors -> UpdateCivM Income
cityIncome gamename playername coors = do
	incomes <- forCityOutskirts gamename playername coors $ \ (outskirt_coors,city,square) -> do
		squareIncome gamename playername outskirt_coors
	return $ mconcat incomes

squareIncome :: GameName -> PlayerName -> Coors -> UpdateCivM Income
squareIncome gamename playername coors = do
	Just (Square{..}) <- getSquare gamename coors
	return $ case null $ filter (/=playername) (concatMap snd (tokenStackToList _squareFigures)) of
		False -> noIncome
		True  -> case _squareTokenMarker of
			Just (BuildingMarker (Building buildingtype pn)) | pn==playername -> generatedIncome buildingtype
			Just _ -> noIncome
			_ ->
				(if _squareNatWonder then cultureIncome 1 else noIncome) +#
				(if _squareCoin then oneCoin else noIncome) +#
				(maybe noIncome (resourceIncome.(:[]).One) _squareResource) +#
				(generatedIncome (Prelude.head _squareTerrain))

getTrade :: GameName -> PlayerName -> UpdateCivM ()
getTrade gamename playername = do
	incomes <- forAllOutskirts gamename playername $ \ (coors,_,_) -> do
		squareIncome gamename playername coors
	addTrade (inTrade $ mconcat incomes) gamename playername

addCulture :: Culture -> GameName -> PlayerName -> UpdateCivM ()
addCulture culture gamename playername = do
	updateCivLensM (addCultureDial culture) $ civPlayerLens gamename playername . _Just . playerCulture

addTrade :: Trade -> GameName -> PlayerName -> UpdateCivM ()
addTrade trade gamename playername = do
	updateCivLensM (addTradeDial trade) $ civPlayerLens gamename playername . _Just . playerTrade

drawPolicy :: GameName -> PlayerName -> Policy -> UpdateCivM ()
drawPolicy gamename playername policy = do
	updateCivLensM (\ (cs,ps) -> (delete (policy2Card policy) cs,policy:ps)) $
		civPlayerLens gamename playername . _Just . playerPolicies

returnPolicy :: GameName -> PlayerName -> Policy -> UpdateCivM ()
returnPolicy gamename playername policy = do
	updateCivLensM (\ (cs,ps) -> (policy2Card policy : cs,delete policy ps)) $
		civPlayerLens gamename playername . _Just . playerPolicies

advanceCulture :: GameName -> PlayerName -> UpdateCivM ()
advanceCulture gamename playername = do
	updateCivLensM (+1) $ civPlayerLens gamename playername . _Just . playerCultureSteps
	Just steps <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerCultureSteps
	when (steps >= 21) $ victory CultureVictory gamename playername

drawCultureCard :: GameName -> PlayerName -> UpdateCivM ()
drawCultureCard gamename playername = do
	advanceCulture gamename playername
	Just steps <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerCultureSteps
	let (culture,trade) = cultureStepCost steps
	addCulture (-culture) gamename playername
	addTrade (-trade) gamename playername
	case cultureStep steps of
		Nothing -> return ()
		Just DrawGreatPerson -> do
			getGreatPerson gamename playername
		Just (DrawCultureCard level) -> do
			getCultureCard gamename playername level

drawUnit :: GameName -> PlayerName -> UnitType -> UpdateCivM Bool
drawUnit gamename playername unittype = do
	mb_unit <- takeFromStackM (civGameLens gamename . _Just . gameUnitStack) unittype
	case mb_unit of
		Nothing -> return False
		Just unit -> do
			updateCivLensM (unit:) $ civPlayerLens gamename playername . _Just . playerUnits
			return True

returnUnit :: GameName -> PlayerName -> UnitCard -> UpdateCivM ()
returnUnit gamename playername unit = do
	updateCivLensM (delete unit) $ civPlayerLens gamename playername . _Just . playerUnits
	putOnStackM (civGameLens gamename . _Just . gameUnitStack) (unitType unit) unit

getFigure :: Figure ->GameName -> PlayerName ->  UpdateCivM ()
getFigure figure gamename playername = do
	mb_figure <- takeFromStackM (civPlayerLens gamename playername . _Just . playerFigures) figure
	case mb_figure of
		Nothing -> return ()
		Just () -> return () -- TODO

getGreatPerson :: GameName -> PlayerName -> UpdateCivM ()
getGreatPerson gamename playername = do
	Just greatperson <- takeFromStackM (civGameLens gamename . _Just . gameGreatPersonStack) ()
	updateCivLensM ((GreatPersonCard greatperson False):) $ civPlayerLens gamename playername . _Just . playerGreatPersonCards

getCultureCard :: GameName -> PlayerName -> CultureLevel -> UpdateCivM ()
getCultureCard gamename playername level = do
	Just cultureevent <- takeFromStackM (civGameLens gamename . _Just . gameCultureStack) level
	updateCivLensM ((CultureCard False cultureevent (Coins 0)):) $ civPlayerLens gamename playername . _Just . playerCultureCards

getResource :: GameName -> PlayerName -> Resource -> UpdateCivM ()
getResource gamename playername resource = do
	Just () <- takeFromStackM (civGameLens gamename . _Just . gameResourceStack) resource
	updateCivLensM (resource:) $ civPlayerLens gamename playername . _Just . playerItems . _1

returnResource :: GameName -> PlayerName -> Resource -> UpdateCivM ()
returnResource gamename playername resource = do
	updateCivLensM (delete resource) $ civPlayerLens gamename playername . _Just . playerItems . _1
	putOnStackM (civGameLens gamename . _Just . gameResourceStack) resource ()

getHut :: GameName -> PlayerName -> Hut -> UpdateCivM ()
getHut gamename playername hut = do
	updateCivLensM (hut:) $ civPlayerLens gamename playername . _Just . playerItems . _2

returnHut :: GameName -> PlayerName -> Hut -> UpdateCivM ()
returnHut gamename playername hut = do
	updateCivLensM (delete hut) $ civPlayerLens gamename playername . _Just . playerItems . _2

getVillage :: GameName -> PlayerName -> Village -> UpdateCivM ()
getVillage gamename playername village = do
	updateCivLensM (village:) $ civPlayerLens gamename playername . _Just . playerItems . _3

returnVillage :: GameName -> PlayerName -> Village -> UpdateCivM ()
returnVillage gamename playername village = do
	updateCivLensM (delete village) $ civPlayerLens gamename playername . _Just . playerItems . _3

getArtifact :: GameName -> PlayerName -> Artifact -> UpdateCivM ()
getArtifact gamename playername artifact = do
	updateCivLensM (artifact:) $ civPlayerLens gamename playername . _Just . playerItems . _4

returnArtifact :: GameName -> PlayerName -> Artifact -> UpdateCivM ()
returnArtifact gamename playername artifact = do
	updateCivLensM (delete artifact) $ civPlayerLens gamename playername . _Just . playerItems . _4

addCoins :: Coins -> GameName -> PlayerName -> UpdateCivM ()
addCoins coins gamename playername = do
	updateCivLensM (+coins) $ civPlayerLens gamename playername . _Just . playerCoins

addTech :: GameName -> PlayerName -> Maybe TechLevel -> Tech -> UpdateCivM ()
addTech gamename playername mb_level tech = do
	let techlevel = case mb_level of
		Nothing -> levelOfTech tech
		Just level -> level
	updateCivLensM ((TechCard tech techlevel (Coins 0)):) $
		civPlayerLens gamename playername . _Just . playerTechs
	when (tech==SpaceFlight) $ updateCivLensM (const True) $
		civGameLens gamename . _Just . gameSpaceFlightTaken

addCoinToTech :: Tech -> GameName -> PlayerName -> UpdateCivM ()
addCoinToTech tech gamename playername = do
	updateCivLensM addcoin $ civPlayerLens gamename playername . _Just . playerTechs
	where
	addcoin [] = error $ "addCoinToTech: Couldn't find " ++ show tech
	addcoin (techcard:ts) | _techCardTechId techcard == tech =
		techcard { _techCardCoins = _techCardCoins techcard + 1 } : ts 
	addcoin (t:ts) = t : addcoin ts

setGovernment :: Government -> GameName -> PlayerName -> UpdateCivM ()
setGovernment government gamename playername = do
	updateCivLensM (const government) $ civPlayerLens gamename playername . _Just . playerGovernment

setShuffledPlayers :: GameName -> Players -> Update CivState UpdateResult
setShuffledPlayers gamename players = runUpdateCivM $ do
	updateCivLensM (const players) $ civPlayersLens gamename

updateBoard :: GameName -> [(Coors,Square)] -> UpdateCivM ()
updateBoard gamename coorsquares = do
	updateCivLensM (// coorsquares) $ civGameLens gamename . _Just . gameBoard

debugShowBoard txt gamename = do
	Just board <- queryCivLensM (civGameLens gamename . _Just . gameBoard)
	error $ unlines $ txt : map show (assocs board)

createBoard :: GameName -> UpdateCivM ()
createBoard gamename = do
	Just players <- queryCivLensM $ civPlayersLens gamename
	let
		layout = boardLayout (numPlayers players)
		lcoors = map fst layout
		(lxcoors,lycoors) = (map xCoor lcoors,map yCoor lcoors)
		lower = Coors (Prelude.minimum lxcoors) (Prelude.minimum lycoors)
		upper = Coors (Prelude.maximum lxcoors + 3) (Prelude.maximum lycoors + 3)
	updateCivLensM (const $ array (lower,upper) (zip (range (lower,upper)) $ repeat OutOfBounds)) $
		civGameLens gamename . _Just . gameBoard
	forM_ layout $ \ (coors,layouttile) -> do
		case layouttile of
			NT -> do
				Just tid <- takeFromStackM (civGameLens gamename . _Just . gameTileStack) ()
				updateBoard gamename $ squaresfromtile tid coors
--				revealTile gamename coors Northward
			CT playerindex ori -> do
				Just (playername,player) <- queryCivLensM $ civPlayerIndexLens gamename playerindex
				updateBoard gamename $ squaresfromtile (Tile $ _playerCiv player) coors
				updateCivLensM (const ori) $ civPlayerLens gamename playername . _Just . playerOrientation
				revealTile gamename coors ori
				let firstcitycoors = [ coors +/+ (Coors xo yo) | xo <- [1..2], yo <- [1..2] ]
				updateCivLensM (const firstcitycoors) $
					civPlayerLens gamename playername . _Just . playerFirstCityCoors
	where
	squaresfromtile :: TileID -> Coors -> [(Coors,Square)]
	squaresfromtile tileid tilecoors = (flip map) (tileSquares tileid) $
		\ (tcoors,_) -> (tilecoors +/+ tcoors,UnrevealedSquare tileid tilecoors)

takeFromStackM :: (Ord toktyp) => Traversal' CivState (TokenStack toktyp tok) -> toktyp -> UpdateCivM (Maybe tok)
takeFromStackM stacklens toktyp = do
	Just stack <- queryCivLensM stacklens
	case takeFromStack toktyp stack of
		Nothing -> return Nothing
		Just (tok,stack') -> do
			updateCivLensM (\_-> stack') stacklens
			return $ Just tok

putOnStackM :: (Ord toktyp) => Traversal' CivState (TokenStack toktyp tok) -> toktyp -> tok -> UpdateCivM ()
putOnStackM stacklens toktyp tok = do
	updateCivLensM (putOnStack toktyp tok) stacklens

revealTile :: GameName -> Coors -> Orientation -> UpdateCivM ()
revealTile gamename coors orientation = do
	mb_sq <- getSquare gamename coors
	(UnrevealedSquare tileid tilecoors) <- case mb_sq of
		Just sq@(UnrevealedSquare _ _) -> return sq
		_ -> do
			debugShowBoard (show coors ++ show mb_sq) gamename

	coorssquares <- forM (tileSquares tileid) $ \ (tcoors,sq) -> do
		sq' <- case _squareTokenMarker sq of
			Just (HutMarker _) -> do
				mb_hut <- takeFromStackM (civGameLens gamename . _Just . gameHutStack) ()
				return $ sq { _squareTokenMarker = fmap HutMarker mb_hut }
			Just (VillageMarker _) -> do
				mb_village :: Maybe Village <- takeFromStackM (civGameLens gamename . _Just . gameVillageStack) ()
				return $ sq { _squareTokenMarker = fmap VillageMarker mb_village }
			_ -> return sq
		let sqcoors = tilecoors +/+ (rotate4x4coors orientation tcoors)
		let tileidori = case tilecoors==sqcoors of
			True  -> Just (tileid,orientation)
			False -> Nothing
		return (sqcoors,sq' { _squareTileIDOri = tileidori } )
	updateBoard gamename coorssquares

getSquare :: GameName -> Coors -> UpdateCivM (Maybe Square)
getSquare gamename coors = do
	queryCivLensM $ civSquareLens gamename coors

type UpdateResult = Either String ()

oK = Right ()
eRR errmsg = Left errmsg

runUpdateCivM :: UpdateCivM () -> Update CivState UpdateResult
runUpdateCivM = runErrorT

updateCivLensM :: (val -> val) -> Traversal' CivState val -> UpdateCivM ()
updateCivLensM fval lens = modify $ over lens fval

queryCivLensM :: (MonadState CivState m) => Traversal' CivState a -> m (Maybe a)
queryCivLensM lens = do
	civstate <- Control.Monad.State.get
	return $ preview lens civstate

buildCity :: GameName -> Coors -> City -> UpdateCivM ()
buildCity gamename coors city@(City{..}) = do
	Just () <- takeFromStackM (civPlayerLens gamename _cityOwner . _Just . playerCityStack) ()
	updateCivLensM (const $ Just $ CityMarker city) $ civSquareLens gamename coors . squareTokenMarker
	updateCivLensM (++[coors]) $ civPlayerLens gamename _cityOwner . _Just . playerCityCoors
	case _cityMetropolisOrientation of
		Nothing  -> return ()
		Just ori -> do
			updateCivLensM (const $ Just $ CityMarker $ SecondCitySquare ori) $
				civSquareLens gamename (addCoorsOri coors ori) . squareTokenMarker

buildBuilding :: GameName -> PlayerName -> Coors -> BuildingType -> UpdateCivM ()
buildBuilding gamename playername coors buildingtype = do
	Just () <- takeFromStackM (civGameLens gamename . _Just . gameBuildingStack) (buildingTypeToMarker buildingtype)
	updateCivLensM (const $ Just $ BuildingMarker $ Building buildingtype playername) $
		civSquareLens gamename coors . squareTokenMarker

victory victorytype gamename playername = error "Not implemented yet"

canStayMoveOn :: (MovementType -> [Terrain]) -> GameName -> PlayerName -> Coors -> UpdateCivM Bool
canStayMoveOn mtterrains gamename playername coors = do
	Just (square@(Square{..})) <- getSquare gamename coors
	Just (player@(Player{..})) <- getPlayer gamename playername
	let stacklimit = getValueAbility unitStackLimit player
	let terrains = mtterrains $ maximum $ map movementType $ playerAbilities player
	return $ 
		stacklimit > length (figuresOfPlayerOnSquare playername square) &&
		(head _squareTerrain) `elem` terrains

canStayOn = canStayMoveOn movementTypeEndTerrains
canCross  = canStayMoveOn movementTypeCrossTerrains

buildFigureCoors gamename playername citycoors = do
	outskirts <- outskirtsOfCity gamename citycoors
	filterM (canStayOn gamename playername) outskirts

doMove :: GameName -> PlayerName -> Move -> UpdateCivM ()
doMove gamename playername move@(Move source target) = do
	Game{..} <- getGame gamename
	case (source,target) of
		(CitySource pn1,BuildFirstCityTarget pn2 coors) | pn1==playername && pn2==playername -> do
			buildCity gamename coors $ newCity playername True Nothing
		(AutomaticMove (),GetTradeTarget pn) | pn==playername ->
			getTrade gamename playername
		(FigureSource pn figure,SquareTarget coors) | pn==playername ->
			buildFigure gamename playername figure coors
		(CityProductionSource _ production,SquareTarget coors) -> case production of
			ProduceFigure figure -> buildFigure gamename playername figure coors
			ProduceBuilding building -> buildBuilding gamename playername coors building
		_ -> error $ show move ++ " not implemented yet"
	updateCivLensM (addmove _gameTurn _gamePhase) $ civPlayerLens gamename playername . _Just . playerMoves
	return ()
	where
	addmove turn phase =
		Map.insertWith (Map.unionWith (++)) turn (Map.singleton phase [move])

moveGenM :: GameName -> PlayerName -> UpdateCivM [Move]
moveGenM gamename my_playername = Import.lift $ moveGen gamename my_playername

moveGen :: GameName -> PlayerName -> Update CivState [Move]
moveGen gamename my_playername = do
	Right moves <- runErrorT $ do
		playername <- getPlayerTurn gamename
		Game{..} <- getGame gamename
		Just (player@(Player{..})) <- queryCivLensM $ civPlayerLens gamename playername . _Just
		moves <- case my_playername == playername of
			False -> return []
			True -> do
				case _gamePhase of
					StartOfGame -> return []
					BuildingFirstCity -> return $
						[ Move (CitySource my_playername) (BuildFirstCityTarget my_playername coors) | coors <- _playerFirstCityCoors ]
					PlaceFirstFigures -> do
						possible_squares <- buildFigureCoors gamename playername (head _playerCityCoors)
						let possible_figures = [Wagon,Flag] -- TODO: Kosten berechnen bei BuildFigures
						return [ Move (FigureSource my_playername figure) (SquareTarget coors) | coors <- possible_squares, figure <- possible_figures ]
					GettingFirstTrade ->
						return [ Move (AutomaticMove ()) (GetTradeTarget my_playername) ]
					StartOfTurn ->
						return []
					Trading ->
						return [ Move (AutomaticMove ()) (GetTradeTarget my_playername) ]
					CityManagement -> do
						movess <- forAllCities gamename my_playername $ \ (citycoors,city) -> do
							income <- cityIncome gamename my_playername citycoors

							possible_fig_coors <- buildFigureCoors gamename playername citycoors
							let
								possible_figures = map fst $ filter ((>0).snd) $ tokenStackHeights _playerFigures
								prodfiguremoves = 
								[ Move (CityProductionSource citycoors (ProduceFigure fig)) (SquareTarget squarecoors) |
									fig <- filter ((<=income) . consumedIncome) possible_figures,
									squarecoors <- possible_fig_coors ]

								player_buildings = concatMap enabledBuildings $ playerAbilities player
								buildings_left = concatMap buildingMarkerToType $ map fst $ filter ((>0).snd) $ tokenStackHeights _gameBuildingStack
								all_avail_buildings = buildings_left `intersect` player_buildings
								avail_downgraded = [ dg | (_,[dg,ug]) <- buildingMarkerType, ug `elem` all_avail_buildings ]
								avail_buildings = filter (`notElem` avail_downgraded) all_avail_buildings
								affordable_buildings = filter (--HERE--) avail_buildings
							return $ prodfiguremoves ++ prodbuildingmoves

						return $ concat movess
					_ ->
						return [ Move (HaltSource ()) (NoTarget ()) ]   -- TODO: Entfernen...
		mb_movesthisphase <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerMoves . at turn . _Just . at phase . _Just
		let movesthisphase = maybe [] Prelude.id mb_movesthisphase
		--foldl :: (a -> b -> a) -> a -> [b] -> a
		return $ foldl (\ allowedmoves move1 -> filter (allowSecondMove move1) allowedmoves) moves movesthisphase
	return moves

setDbgMessage :: String -> UpdateCivM ()
setDbgMessage msg = updateCivLensM (const msg) civDebugMsg

checkMovesLeft :: GameName -> UpdateCivM ()
checkMovesLeft gamename = do
	playername <- getPlayerTurn gamename
	moves_left <- moveGenM gamename playername
	case moves_left of
		[] -> do
			finishPlayerPhase gamename
			checkMovesLeft gamename
		[move@(Move (AutomaticMove ()) _)] -> do
			doMove gamename playername move
			checkMovesLeft gamename
		_ -> return ()


$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'createNewGame,
	'gameAction,
	'moveGen
	])

