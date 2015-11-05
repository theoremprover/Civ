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
import System.Random

import Logic
import Lenses
import TokenStack
import Model
import Polls
import ModelVersion
import AssocList


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
	battleHandSize           :: HookM (Value Int),
	cultureCardLimit         :: HookM (Value Int),
	cultureTrackBonus        :: HookM (Value Income),
	researchCostBonus   :: Value Income,
	unitStackLimit      :: Value Int,
	moveRange           :: Value Coor,
	movementType        :: Value MovementType,
	cardCoins           :: Coins,
	threeTradeHammers   :: Value Int,
	productionBonus     :: HookM (Value Income),
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
	cardAbilities       :: Phase -> [(Tech,String,HookM ())],
	resourceAbilities   :: Phase -> [(Tech,String,[ResourcePattern],HookM ())] }

defaultAbilities = Abilities {
	unitLevel       = \case
		Aircraft -> SetValue Nothing
		_        -> SetValue (Just UnitLevelI),
	unitAttackBonus = const 0,
	lootBonus       = SetValue 0,
	battleStrengthBonus = SetValue 0,
	battleHandSize  = constHookM $ SetValue 3,
	cultureCardLimit = constHookM $ SetValue 2,
	cultureTrackBonus = constHookM $ SetValue noIncome,
	researchCostBonus = SetValue noIncome,
	unitStackLimit  = SetValue 2,
	moveRange       = SetValue 2,
	movementType    = SetValue Land,
	cardCoins       = Coins 0,
	threeTradeHammers = SetValue 1,
	productionBonus = constHookM $ SetValue noIncome,
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
	battleHandSize  = constHookM Unchanged,
	cultureCardLimit = constHookM Unchanged,
	cultureTrackBonus = constHookM Unchanged,
	researchCostBonus = Unchanged,
	unitStackLimit  = Unchanged,
	moveRange       = Unchanged,
	movementType    = Unchanged,
	cardCoins       = Coins 0,
	threeTradeHammers = Unchanged,
	productionBonus = constHookM Unchanged,
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
		cardAbilities = cardAbility (CivAbility Egypt) [CityManagement] "Egypt: Free Building" freeBuilding_Egypt $ const [] }
	English  -> defaultAbilities {
		movementType = SetValue CrossWater,
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
		researchCostBonus = SetValue (tradeIncome 3),
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
		startOfGameHook  = oneMoreFigure Flag,
		unitStackLimit   = ModifyValue (+1),
		sacrificeForTech = SetValue True }
	Spanish  -> defaultAbilities {
		startOfGameHook = oneMoreFigure Wagon,
		moveRange       = ModifyValue (+1),
		exploreTileHook = buildUnlockedBuilding_Spanish }
	Zulu     -> defaultAbilities {
		startOfGameHook         = \ gn pn -> forM_ [Artillery,Artillery] $ drawUnit gn pn,
		exploreHutWithoutBattle = SetValue True,
		buildCityNextToHuts     = SetValue True }

techIdAbility = case _techCardTechId of
	Pottery              -> unchangedAbilities {
		enabledBuildings   = [Granary],
		cultureCardLimit   = constHookM $ ModifyValue (+1),
		resourceAbilities  = resourceAbility Pottery [CityManagement] "Gain Coin" [AnyResource,AnyResource] (addCoinToTech Pottery) $ const [] }
	Writing              -> unchangedAbilities {
		enabledBuildings   = [Library],
		resourceAbilities  = resourceAbility Writing [CityManagement] "Cancel City Action" [One Spy] cancelCityAction_Writing $ const [] }
	CodeOfLaws           -> unchangedAbilities {
		enabledGovernments = [Republic],
		enabledBuildings   = [TradePost],
		cardAbilities      = cardAbility (CardAbilityTarget CodeOfLaws) [Research] "Add Coin" addCoinAfterWonBattle_CodeOfLaws $ const [] }
	Currency             -> unchangedAbilities {
		enabledBuildings   = [Market],
		resourceAbilities  = resourceAbility Currency [CityManagement] "Gain 3 Culture" [One Incense] (addCulture 3) $ const [] }
	Metalworking         -> unchangedAbilities {
		enabledBuildings   = [Barracks],
		resourceAbilities  = resourceAbility Metalworking [Battle] "Increase Attack" [One Iron] increaseAttack_Metalworking $ const [] }
	Masonry              -> unchangedAbilities {
		unitStackLimit     = SetValue 3,
		cardAbilities      = cardAbility (CardAbilityTarget Masonry) [CityManagement] "Build City Walls" buildCityWalls_Masonry $ const [] }		
	Agriculture          -> unchangedAbilities {
		getThisHook        = growIntoMetropolisHook_Agriculture,
		canBuildMetropolis = SetValue True }
	HorsebackRiding      -> unchangedAbilities {
		moveRange          = SetValue 3,
		resourceAbilities  = resourceAbility HorsebackRiding [Trading] "Get Trade" [One Linen] getTrade_HoresebackRiding $ const [] }
	AnimalHusbandry      -> unchangedAbilities {
		cardAbilities      = cardAbility (CardAbilityTarget AnimalHusbandry) [Battle] "Heal 3 Damage" (healDamage 3) $ const [],
		resourceAbilities  = resourceAbility AnimalHusbandry [CityManagement] "+3 Hammers" [One Wheat] (plusHammers 3) $ const [] }
	Philosophy           -> unchangedAbilities {
		enabledBuildings   = [Temple],
		resourceAbilities  = resourceAbility Philosophy [CityManagement] "Get Great Person" [AnyResource,AnyResource,AnyResource] getGreatPerson $ const [] }
	Navigation           -> unchangedAbilities {
		enabledBuildings   = [Harbour],
		movementType       = SetValue CrossWater }
	Navy                 -> unchangedAbilities {
		enabledBuildings   = [Shipyard],
		buildArmyHook      = armyToShipyardOutskirtsHook_Navy }
	PublicAdministration -> unchangedAbilities {
		cardCoins          = Coins 1,
		cultureCardLimit   = constHookM $ ModifyValue (+1),
		resourceAbilities  = resourceAbility PublicAdministration allPhases "Cancel Culture Event" [One Spy] cancelCultureEvent $ const [] }
	Mysticism            -> unchangedAbilities {
		drawCultureHook    = drawAnotherCultureCardHook_Mysticism,
		resourceAbilities  = resourceAbility Mysticism [CityManagement] "Force Discard Coin" [One Spy] forceDiscardCoin_Mysticism $ const [] }
	MonarchyTech         -> unchangedAbilities {
		enabledGovernments = [Monarchy],
		resourceAbilities  = resourceAbility MonarchyTech [CityManagement] "Destroy Wonder/Unit" [One Linen] destroyWonderOrUnit_MonarchyTech $ const [] }
	DemocracyTech        -> unchangedAbilities {
		unitLevel = setUnitLevel [Infantry] UnitLevelII,
		enabledGovernments = [Democracy],
		cardAbilities      = cardAbility (CardAbilityTarget DemocracyTech) [CityManagement] "Add Coin" addCoin_Democracy $ const [] }
	Chivalry             -> unchangedAbilities {
		unitLevel = setUnitLevel [Cavalry] UnitLevelII,
		enabledGovernments = [Feudalism],
		resourceAbilities  = resourceAbility Chivalry [CityManagement] "Gain 5 Culture" [One Incense] (addCulture 5) $ const [] }
	Mathematics          -> unchangedAbilities {
		unitLevel = setUnitLevel [Artillery] UnitLevelII,
		resourceAbilities  = resourceAbility Mathematics [Battle] "Deal 3 Damage" [One Iron] (dealDamage 3) $ const [] }
	Logistics            -> unchangedAbilities {
		unitLevel = setUnitLevel [Infantry,Cavalry,Artillery] UnitLevelII }
	PrintingPress        -> unchangedAbilities {
		unitStackLimit     = SetValue 4,
		enabledBuildings   = [University],
		cardAbilities      = cardAbility (CardAbilityTarget PrintingPress) [CityManagement] "Add Coin" addCoin_PrintingPress $ const [] }
	Sailing              -> unchangedAbilities {
		moveRange          = SetValue 4,
		movementType       = SetValue StayInWater }
	Construction         -> unchangedAbilities {
		enabledBuildings   = [Forge],
		resourceAbilities  = resourceAbility Construction [CityManagement] "+5 Hammers" [One Wheat] (plusHammers 5) $ const [] }
	Engineering          -> unchangedAbilities {
		enabledBuildings   = [Aquaeduct],
		cardAbilities      = cardAbility (CardAbilityTarget Engineering) [CityManagement] "Split Production" splitProduction_Engineering $ const [] }
	Irrigation           -> unchangedAbilities {
		getThisHook        = pushThirdCity }
	Bureaucracy          -> unchangedAbilities {
		cardCoins          = Coins 1,
		cardAbilities      = cardAbility (CardAbilityTarget Bureaucracy) [Research] "Switch Policy" switchPolicy_Bureaucracy $ const [] }
	Theology             -> unchangedAbilities {
		enabledGovernments = [Fundamentalism],
		enabledBuildings   = [Cathedral],
		cultureCardLimit   = constHookM $ ModifyValue (+1) }
	CommunismTech        -> unchangedAbilities {
		enabledGovernments = [Communism],
		cardAbilities      = cardAbility (CardAbilityTarget CommunismTech) [Movement] "Lock Square" lockSquare_CommunismTech $ const [] }
	Gunpowder            -> unchangedAbilities {
		unitLevel          = setUnitLevel [Infantry] UnitLevelIII,
		resourceAbilities  = resourceAbility Gunpowder [CityManagement] "Destroy Wonder/Buliding" [AnyResource,AnyResource] destroyWonderBuilding_Gunpowder $ const [] }
	Railroad             -> unchangedAbilities {
		enabledBuildings   = [IronMine],
		cardCoins          = Coins 1,
		unitLevel          = setUnitLevel [Cavalry] UnitLevelIII }
	MetalCasting         -> unchangedAbilities {
		unitLevel          = setUnitLevel [Artillery] UnitLevelIII,
		resourceAbilities  = resourceAbility MetalCasting [CityManagement] "Gain 7 Culture" [One Incense] (addCulture 7) $ const [] }
	Ecology              -> unchangedAbilities {
		cultureTrackBonus  = modifyValuePerNCoins 3 ((+#).tradeIncome),
		resourceAbilities  = resourceAbility Ecology [StartOfTurn] "Change Terrain" [One Wheat] (changeTerrain_Ecology) $ const [] }
	Biology              -> unchangedAbilities {
		unitStackLimit     = SetValue 5,
		cardAbilities      = cardAbility (CardAbilityTarget Biology) [Battle] "Heal All Damage" healAllDamage $ const [] }
	SteamEngine          -> unchangedAbilities {
		moveRange          = SetValue 5,
		movementType       = SetValue StayInWater,
		resourceAbilities  = resourceAbility SteamEngine [CityManagement] "Move Figures" [One Linen] moveFigures_SteamEngine $ const [] }
	Banking              -> unchangedAbilities {
		resourceAbilities  = resourceAbility Banking [CityManagement] "+7 Hammers" [One Wheat] (plusHammers 7) $ const [],
		enabledBuildings   = [Bank] }
	MilitaryScience      -> unchangedAbilities {
		enabledBuildings   = [Academy],
		productionBonus    = modifyValuePerNCoins 3 ((+#).hammerIncome) }
	Education            -> unchangedAbilities {
		buildWonderHook    = addCoinToCard Education,
		resourceAbilities  = resourceAbility Education [CityManagement] "Learn Tech" [One Wheat,One Incense,One Iron,One Linen] learnTech_Education $ const [] }
	Computers            -> unchangedAbilities {
		cardCoins          = Coins 1,
		battleHandSize     = modifyValuePerNCoins 5 (+),
		cultureCardLimit   = modifyValuePerNCoins 5 (+) }
	MassMedia            -> unchangedAbilities {
		cardAbilities      = cardAbility (CardAbilityTarget MassMedia) allPhases "Immune Culture Events" cancelCultureEventCancel_MassMedia $ const [],
		resourceAbilities  = resourceAbility MassMedia allPhases "Cancel Resource Ability" [One Spy] cancelResourceAbility_MassMedia $ const [] }
	Ballistics           -> unchangedAbilities {
		unitLevel          = setUnitLevel [Artillery] UnitLevelStar,
		resourceAbilities  = resourceAbility Ballistics [Battle] "Deal 6 Damage" [One Iron] (dealDamage 6) $ const [] }
	ReplacementParts     -> unchangedAbilities {
		unitStackLimit     = SetValue 6,
		unitLevel          = setUnitLevel [Infantry] UnitLevelStar }
	Flight               -> unchangedAbilities {
		unitLevel          = setUnitLevel [Aircraft] UnitLevelStar,
		moveRange          = SetValue 6,
		movementType       = SetValue Air }
	Plastics             -> unchangedAbilities {
		cardAbilities      = cardAbility (CardAbilityTarget Plastics) [StartOfTurn] "Build Unit/Figure/Building" buildUnitFigureBuilding_Plastics $ const [],
		resourceAbilities  = resourceAbility Plastics [CityManagement] "+10 Hammers" [One Wheat] (plusHammers 10) $ const [] }
	CombustionEngine     -> unchangedAbilities {
		unitLevel          = setUnitLevel [Cavalry] UnitLevelStar,
		cardAbilities      =
			cardAbility CombustionEngine [Movement] "Destroy Building" destroyBuilding_CombustionEngine $
			cardAbility CombustionEngine [Battle] "Destroy Walls" destroyWalls_CombustionEngine $ const [] }
	AtomicTheory         -> unchangedAbilities {
		resourceAbilities  =
			resourceAbility AtomicTheory [CityManagement] "Additional City Actions" [One Atom] additionalCityActions_AtomicTheory $
			resourceAbility AtomicTheory [Movement] "Nuke City" [One Atom] nukeCity_AtomicTheory $ const [] }
	SpaceFlight          -> unchangedAbilities {
		getThisHook        = \ gamename playername -> do
			updateCivLensM (const True) $ civGameLens gamename . _Just . gameSpaceFlightTaken
			victory TechVictory gamename playername }

	where

	setUnitLevel unittypes unitlevel ut | ut `elem` unittypes = SetValue (Just unitlevel)
	setUnitLevel _ _ _ = Unchanged

	resourceAbility tech phases name respats action f phase | phase `elem` phases = [(tech,name,respats,action)]
	resourceAbility _ _ _ _ _ f phase = f phase

techAbilities TechCard{..} = techIdAbility { cardCoins = _techCardCoins + cardCoins techIdAbility }

cardAbility target phases name action f phase | phase `elem` phases = [(target,name,action)]
cardAbility _ _ _ _ f phase = f phase

modifyValuePerNCoins :: Int -> (Int -> a -> a) -> HookM (Value a)
modifyValuePerNCoins n int2af gamename playername = do
	Coins coins <- playerNumCoinsM gamename playername
	return $ ModifyValue $ int2af (mod coins n)

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

pushThirdCity gamename playername = do
	putOnStackM (civPlayerLens gamename playername . _Just . playerCityStack) () ()

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

getValueAbility1 :: (Ord a,Show a) => (Abilities -> arg -> Value a) -> Player -> arg -> a
getValueAbility1 f player arg = getValueAbility (\ abilities -> f abilities arg) player

-- abstrahieren auf ein Argument für ability
getValueAbility :: (Ord a,Show a) => (Abilities -> Value a) -> Player -> a
getValueAbility toability player = valueAbilities $ map toability (playerAbilities player)

getHookValueAbilityM :: (Ord a,Show a) => GameName -> PlayerName -> (Abilities -> HookM (Value a)) -> UpdateCivM a
getHookValueAbilityM gamename playername ability2hook = do
	Just player <- getPlayer gamename playername
	vals <- forM (playerAbilities player) $ \ ability -> do
		(ability2hook ability) gamename playername
	return $ valueAbilities vals

playerAbilities player@(Player{..}) =
	defaultAbilities : civAbilities _playerCiv :
	map techAbilities (concat $ Map.elems _playerTechs)


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

	forAllPlayers gamename $ \ (playername,player@(Player{..})) -> do
		forM_ [Artillery,Infantry,Cavalry] $ drawUnit gamename playername
		let (starttech,startgov) = civStartTechAndGov _playerCiv
		addTech gamename playername (Just TechLevelI) starttech
		setGovernment startgov gamename playername
		forM_ [Incense,Wheat,Linen,Iron] $ \ res -> do
			putOnStackM (civGameLens gamename . _Just . gameResourceStack) res ()
		forM_ (map startOfGameHook (playerAbilities player)) $ \ a -> a gamename playername

	when debugMode $ do
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

autoPlayGame :: GameName -> StdGen -> Update CivState UpdateResult
autoPlayGame gamename randgen = do
	startGame gamename
	runUpdateCivM $ autoPlayLoop gamename randgen
	return oK

autoPlayLoop :: GameName -> StdGen -> UpdateCivM ()
autoPlayLoop gamename randgen = do
	Just (Game{..}) <- getGame gamename
	case _gamePhase of
		Movement -> return ()
		_ -> do
			playername <- getPlayerTurn gamename
			moves <- moveGenM gamename playername
			randgen' <- case moves of
				[] -> do
					finishPlayerPhase gamename
					return randgen
				_  -> do
					let (moveindex,randgen') = randomR (0,length moves - 1) randgen
					doMove gamename playername (moves!!moveindex)
					return randgen'
			autoPlayLoop gamename randgen'

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
		case phase of
			CityManagement -> do
				forAllPlayers gamename $ \ (playername,player) -> do
					forAllCities gamename playername $ \ (coors,_) -> do
						updateCivLensM (const noIncome) $
							civCityLens gamename coors . cityIncomeBonus
				return ()

			Movement -> do
				forAllPlayers gamename $ \ (playername,player) -> do
					let range = getValueAbility moveRange player
					updateCivLensM (Map.map $ \ figure -> figure { _figureRangeLeft = range }) $
						civPlayerLens gamename playername . _Just . playerFiguresOnBoard
				return ()

			StartOfTurn -> do
				updateCivLensM (+1) $ civGameLens gamename . _Just . gameTurn
				when (_gameTurn>1) $ do
					incPlayerIndex gamename gamePlayersTurn
					incPlayerIndex gamename gameStartPlayer

			_ -> return ()

allowSecondMove secondmove move = case (move,secondmove) of
	(Move _ (GetTradeTarget _),Move _ (GetTradeTarget _)) -> False
	(Move _ (BuildFirstCityTarget _ _),Move _ (BuildFirstCityTarget _ _)) -> False
	(Move (FigureSource _ fig1) (SquareTarget _),Move (FigureSource _ fig2) (SquareTarget _)) | fig1==fig2 -> False
	(Move (CityProductionSource coors1 _) _,Move (CityProductionSource coors2 _) _) | coors1==coors2 -> False
	(Move (TechSource _) (TechTreeTarget _),Move (TechSource _) (TechTreeTarget _)) -> False
	(Move (ResourcesSource pn1 _) (TechResourceAbilityTarget _ tech1),Move (ResourcesSource pn2 _) (TechResourceAbilityTarget _ tech2)) | pn1==pn2 && tech1==tech2 -> False
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

oneMoreFigure :: FigureType -> GameName -> PlayerName -> UpdateCivM ()
oneMoreFigure figtype gamename playername = do
	Just figstack <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerFigures
	let nextid = maximum (concat $ tokenStackElems figstack) + 1
	putOnStackM (civPlayerLens gamename playername . _Just . playerFigures) figtype nextid

getFigure gamename playername figureid = do
	Just figure <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerFiguresOnBoard . at figureid
	return figure

buildFigure gamename playername figuretype coors = do
	Just figureid <- takeFromStackM (civPlayerLens gamename playername . _Just . playerFigures) figuretype
	let figure = Figure figuretype coors 0
	updateCivLensM (Map.insert figureid figure) $ civPlayerLens gamename playername . _Just . playerFiguresOnBoard
	placeFigure gamename playername figureid coors

placeFigure gamename playername figureid coors = do
	updateCivLensM (\ (Just figure) -> Just $ figure { _figureCoors = coors }) $ civPlayerLens gamename playername . _Just . playerFiguresOnBoard . at figureid
	updateCivLensM ((playername,figureid):) $ civSquareLens gamename coors . squareFigures

unplaceFigure gamename playername figureid coors = do
	updateCivLensM (delete (playername,figureid)) $ civSquareLens gamename coors . squareFigures

destroyFigure gamename playername figureid = do
	Just Figure{..} <- getFigure gamename playername figureid
	unplaceFigure gamename playername figureid _figureCoors
	updateCivLensM (Map.delete figureid) $ civPlayerLens gamename playername . _Just . playerFiguresOnBoard
	putOnStackM (civPlayerLens gamename playername . _Just . playerFigures) _figureType figureid

gameAction :: GameName -> PlayerName -> Move -> Update CivState UpdateResult
gameAction gamename playername move = runUpdateCivM $ do
	moves <- moveGenM gamename playername
	case move `elem` moves of
		False -> error $ show playername ++ " requested " ++ show move ++ " which is not in 'moves'!"
		True -> do
			doMove gamename playername move
			checkMovesLeft gamename
	return ()

forAllPlayers :: GameName -> ((PlayerName,Player) -> UpdateCivM a) -> UpdateCivM [a]
forAllPlayers gamename action = do
	Just players <- queryCivLensM $ civPlayersLens gamename
	forM (fromAssocList players) action

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

forAllPlayerFigures :: GameName -> PlayerName -> ((FigureID,Figure) -> UpdateCivM a) -> UpdateCivM [a]
forAllPlayerFigures gamename playername action = do
	Just figuremap <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerFiguresOnBoard
	forM (Map.assocs figuremap) action

cityIncome :: GameName -> PlayerName -> Coors -> UpdateCivM Income
cityIncome gamename playername coors = do
	outskirt_incomes <- forCityOutskirts gamename playername coors $ \ (outskirt_coors,city,square) -> do
		squareIncome gamename playername outskirt_coors
	city <- getCity gamename coors
	let city_income = generatedIncome city
	return $ mconcat $ city_income : outskirt_incomes

squareIncome :: GameName -> PlayerName -> Coors -> UpdateCivM Income
squareIncome gamename playername coors = do
	Just (Square{..}) <- getSquare gamename coors
	return $ case null $ filter ((/=playername).fst) _squareFigures of
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

setTrade :: Trade -> GameName -> PlayerName -> UpdateCivM ()
setTrade trade gamename playername = do
	updateCivLensM (const $ addTradeDial trade 0) $ civPlayerLens gamename playername . _Just . playerTrade

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
		Just DrawGreatPerson         -> getGreatPerson gamename playername
		Just (DrawCultureCard level) -> getCultureCard gamename playername level

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

getGreatPerson :: GameName -> PlayerName -> UpdateCivM ()
getGreatPerson gamename playername = do
	Just greatperson <- takeFromStackM (civGameLens gamename . _Just . gameGreatPersonStack) ()
	updateCivLensM ((GreatPersonCard greatperson False):) $ civPlayerLens gamename playername . _Just . playerGreatPersonCards

getCultureCard :: GameName -> PlayerName -> CultureLevel -> UpdateCivM ()
getCultureCard gamename playername level = do
	Just cultureevent <- takeFromStackM (civGameLens gamename . _Just . gameCultureStack) level
	updateCivLensM ((CultureCard False cultureevent (Coins 0)):) $ civPlayerLens gamename playername . _Just . playerCultureCards

returnCultureCard :: GameName -> PlayerName -> CultureCard -> UpdateCivM ()
returnCultureCard gamename playername culturecard = do
	updateCivLensM (delete culturecard) $ civPlayerLens gamename playername . _Just . playerCultureCards
	let culturevent = _cultureCardEvent culturecard
	putOnStackM (civGameLens gamename . _Just . gameReturnedCultureCards)
		(cultureEventLevel culturevent) culturevent

getResource :: GameName -> PlayerName -> Resource -> UpdateCivM ()
getResource gamename playername resource = do
	Just () <- takeFromStackM (civGameLens gamename . _Just . gameResourceStack) resource
	updateCivLensM (resource:) $ civPlayerLens gamename playername . _Just . playerResources

returnResource :: GameName -> PlayerName -> Resource -> UpdateCivM ()
returnResource gamename playername resource = do
	updateCivLensM (delete resource) $ civPlayerLens gamename playername . _Just . playerResources
	putOnStackM (civGameLens gamename . _Just . gameResourceStack) resource ()

getHut :: GameName -> PlayerName -> Hut -> UpdateCivM ()
getHut gamename playername hut = do
	updateCivLensM (hut:) $ civPlayerLens gamename playername . _Just . playerHuts

returnHut :: GameName -> PlayerName -> Hut -> UpdateCivM ()
returnHut gamename playername hut = do
	updateCivLensM (delete hut) $ civPlayerLens gamename playername . _Just . playerHuts

getVillage :: GameName -> PlayerName -> Village -> UpdateCivM ()
getVillage gamename playername village = do
	updateCivLensM (village:) $ civPlayerLens gamename playername . _Just . playerVillages

returnVillage :: GameName -> PlayerName -> Village -> UpdateCivM ()
returnVillage gamename playername village = do
	updateCivLensM (delete village) $ civPlayerLens gamename playername . _Just . playerVillages

getArtifact :: GameName -> PlayerName -> Artifact -> UpdateCivM ()
getArtifact gamename playername artifact = do
	updateCivLensM (artifact:) $ civPlayerLens gamename playername . _Just . playerArtifacts

returnArtifact :: GameName -> PlayerName -> Artifact -> UpdateCivM ()
returnArtifact gamename playername artifact = do
	updateCivLensM (delete artifact) $ civPlayerLens gamename playername . _Just . playerArtifacts

addCoins :: Coins -> GameName -> PlayerName -> UpdateCivM ()
addCoins coins gamename playername = do
	updateCivLensM (+coins) $ civPlayerLens gamename playername . _Just . playerCoins

addTech :: GameName -> PlayerName -> Maybe TechLevel -> Tech -> UpdateCivM ()
addTech gamename playername mb_level tech = do
	let techlevel = case mb_level of
		Nothing -> levelOfTech tech
		Just level -> level
	let techcards = [TechCard tech (Coins 0)]
	updateCivLensM (Just . maybe techcards (++techcards)) $
		civPlayerLens gamename playername . _Just . playerTechs . at techlevel

addCoinToTech :: Tech -> GameName -> PlayerName -> UpdateCivM ()
addCoinToTech tech gamename playername = do
	updateCivLensM (Map.map (map addcoinif)) $ civPlayerLens gamename playername . _Just . playerTechs
	where
	addcoinif techcard = case _techCardTechId techcard == tech of
		False -> techcard
		True -> techcard { _techCardCoins = _techCardCoins techcard + Coins 1 }

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

playernamesOnSquare square = case square of
	Square _ _ _ _ _ _ sqfigures -> nub $ map fst sqfigures
	_ -> []

canBuildCityHere :: GameName -> PlayerName -> Coors -> UpdateCivM Bool
canBuildCityHere gamename playername coors = do
	let cityarea = coors : surroundingSquares 1 coors
	Just player <- getPlayer gamename playername
	let citynexttohuts = getValueAbility buildCityNextToHuts player
	empties <- forM cityarea $ \ cs -> do
		mb_square <- getSquare gamename cs
		return $ case mb_square of
			Nothing -> False
			Just square -> case square of
				Square _ (terrain:_) _ _ _ mbtokmark _ ->
					((citynexttohuts && isHut mbtokmark) || isNothing mbtokmark) &&
					terrain `elem` (allOfThem \\ [Water]) &&
					null (playernamesOnSquare square \\ [playername])
				_ -> False
	return $ all (==True) empties

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
	setTokenMarker gamename (BuildingMarker $ Building buildingtype playername) coors

victory victorytype gamename playername = error "Not implemented yet"

modifyRange :: GameName -> PlayerName -> FigureID -> (Coor -> Coor) -> UpdateCivM ()
modifyRange gamename playername figureid rangef = do
	updateCivLensM rangef $ civPlayerLens gamename playername . _Just . playerFiguresOnBoard . at figureid . _Just . figureRangeLeft

setTokenMarker gamename tokenmarker coors = do
	updateCivLensM (const $ Just tokenmarker) $ civSquareLens gamename coors . squareTokenMarker

clearTokenMarker gamename coors = do
	updateCivLensM (const Nothing) $ civSquareLens gamename coors . squareTokenMarker

moveFigure :: GameName -> PlayerName -> FigureID -> Coors -> UpdateCivM ()
moveFigure gamename playername figureid targetcoors = do
	Just Figure{..} <- getFigure gamename playername figureid
	unplaceFigure gamename playername figureid _figureCoors
	let distance = coorDistance _figureCoors targetcoors
	modifyRange gamename playername figureid (+(-distance))
	placeFigure gamename playername figureid targetcoors

	Just Square{..} <- getSquare gamename targetcoors
	case _squareTokenMarker of
		Just (ArtifactMarker artifact) -> do
			modifyRange gamename playername figureid (const 0)
			clearTokenMarker gamename targetcoors
			getArtifact gamename playername artifact
		Just (HutMarker hut) -> do
			modifyRange gamename playername figureid (const 0)
			clearTokenMarker gamename targetcoors
			getHut gamename playername hut
		Just (VillageMarker village) -> do
			modifyRange gamename playername figureid (const 0)
			clearTokenMarker gamename targetcoors
			getVillage gamename playername village
		_ -> return ()

-- Nur definiert für revealed squares!
canStayMoveOn :: (MovementType -> [Terrain]) -> GameName -> PlayerName -> FigureType -> Coors -> UpdateCivM Bool
canStayMoveOn mtterrains gamename playername figuretype coors = do
	Just (square@(Square{..})) <- getSquare gamename coors
	Just (player@(Player{..})) <- getPlayer gamename playername
	let stacklimit = getValueAbility unitStackLimit player
	let terrains = mtterrains $ getValueAbility movementType player
	return $ 
		stacklimit > length (filter ((==playername).fst) _squareFigures) &&
		(head _squareTerrain) `elem` terrains &&
		(not (isVillage _squareTokenMarker) || figuretype==Flag) &&
		(not (isHut _squareTokenMarker) || figuretype==Flag)

canStayOn gamename playername figuretype coors = do
	canstayonterrain <- canStayMoveOn movementTypeEndTerrains gamename playername figuretype coors
	Just Square{..} <- getSquare gamename coors
	return $ canstayonterrain && not (isCity _squareTokenMarker)

canCross = canStayMoveOn movementTypeCrossTerrains

buildFigureCoors gamename playername figuretype citycoors = do
	outskirts <- outskirtsOfCity gamename citycoors
	filterM (canStayOn gamename playername figuretype) outskirts

techCosts player techlevel = consumedIncome techlevel -# getValueAbility researchCostBonus player

doMove :: GameName -> PlayerName -> Move -> UpdateCivM ()
doMove gamename playername move@(Move source target) = do
	Just (Game{..}) <- getGame gamename
	Just player@Player{..} <- getPlayer gamename playername
	case (source,target) of

		(CitySource pn1,BuildFirstCityTarget pn2 coors) | pn1==playername && pn2==playername -> do
			buildCity gamename coors $ newCity playername True Nothing

		(AutomaticMove (),GetTradeTarget pn) | pn==playername -> do
			getTrade gamename playername

		(FigureSource pn figure,SquareTarget coors) | pn==playername -> do
			buildFigure gamename playername figure coors

		(FigureOnBoardSource figureid pn coors,BuildCityTarget ()) | pn==playername -> do
			destroyFigure gamename playername figureid
			buildCity gamename coors $ newCity playername False Nothing

		(CityProductionSource _ (ProduceFigure figure),SquareTarget coors) -> do
			buildFigure gamename playername figure coors

		(CityProductionSource _ (ProduceBuilding building),SquareTarget coors) -> do
			buildBuilding gamename playername coors building

		(CityProductionSource _ (ProduceUnit unittype),NoTarget ()) -> do
			drawUnit gamename playername unittype
			return ()

		(CityProductionSource _ (HarvestResource res),NoTarget ()) -> do
			getResource gamename playername res

		(CityProductionSource _ (DevoteToArts culture),NoTarget ()) -> do
			addCulture culture gamename playername

		(FigureOnBoardSource figureid pn _,SquareTarget targetcoors) | pn==playername -> do
			moveFigure gamename playername figureid targetcoors

		(FigureOnBoardSource figureid pn _,RevealTileTarget ori tileorigin) | pn==playername -> do
			modifyRange gamename playername figureid (+(-1))
			revealTile gamename tileorigin ori

		(TechSource tech,TechTreeTarget pn) | playername==pn -> do
			setTrade (min (inTrade $ techCosts player (levelOfTech tech)) _playerTrade )
				gamename playername
			addTech gamename playername Nothing tech

		(ResourcesSource pn payments,TechResourceAbilityTarget name tech) | pn==playername -> do
			forM_ payments $ \case
				ResourcePayment res            -> returnResource gamename playername res
				CultureCardPayment culturecard -> returnCultureCard gamename playername culturecard
				VillagePayment village         -> returnVillage gamename playername village
				HutPayment hut                 -> returnHut gamename playername hut
			let [(_,_,_,hook)] = filter (\ (_,nm,_,_) -> nm==name) $ resourceAbilities (techIdAbility tech) 
			hook gamename playername

		(NoSource (),CardAbilityTarget name cardtarget) -> do
			let [(_,name,hook)] = filter (\ (_,nm,_) -> nm==name) $ case cardtarget of
				TechCardAbility tech -> cardAbilities (techIdAbility tech)
				CivAbility civ       -> cardAbilities (civAbilities civ)
			hook gamename playername

		(_,FinishPhaseTarget ()) -> finishPlayerPhase gamename

		(_,DebugTarget msg) -> return ()

		_ -> error $ show move ++ " not implemented yet"

	updateCivLensM (addmove _gameTurn _gamePhase) $ civPlayerLens gamename playername . _Just . playerMoves
	return ()
	where
	addmove turn phase =
		Map.insertWith (Map.unionWith (++)) turn (Map.singleton phase [move])

playerNumCoins :: GameName -> PlayerName -> Update CivState Coins
playerNumCoins gamename playername = do
	res <- runErrorT $ playerNumCoinsM gamename playername
	case res of
		Right coins -> return coins
		Left msg -> error msg

playerNumCoinsM :: GameName -> PlayerName -> UpdateCivM Coins
playerNumCoinsM gamename playername = do
	incomess <- forAllCities gamename playername $ \ (coors,_) -> cityIncome gamename playername coors
	Just player@(Player{..}) <- getPlayer gamename playername
	return $
		inCoins (mconcat incomess) +
		_playerCoins +
		sum (map cardCoins (playerAbilities player))

possiblePayments :: Player -> [ResourcePattern] -> [[ResourcePayment]]
possiblePayments Player{..} requiredpats = nub $ map sort $ poss_pays availpays requiredpats []
	where
	availpays = catMaybes $
		map paymentResource    _playerResources ++
		map paymentCultureCard _playerCultureCards ++
		map paymentHut         _playerHuts ++
		map paymentVillage     _playerVillages
	poss_pays _ [] acc = [acc]
	poss_pays avails (pat : reqs) acc = concatMap
		(\ ar@(_,payment) -> poss_pays (delete ar avails) reqs (payment:acc)) $
		filter (case pat of
			One res -> elem res . fst
			AnyResource -> const True)
			avails

moveGenM :: GameName -> PlayerName -> UpdateCivM [Move]
moveGenM gamename playername = Import.lift $ moveGen gamename playername

moveGen :: GameName -> PlayerName -> Update CivState [Move]
moveGen gamename my_playername = do
	res <- runErrorT $ do
		playername <- getPlayerTurn gamename
		Just (Game{..}) <- getGame gamename
		Just (player@(Player{..})) <- queryCivLensM $ civPlayerLens gamename playername . _Just
		moves <- case my_playername == playername of
			False -> return []
			True -> do
				phasemoves <- case _gamePhase of
					StartOfGame -> return []
					BuildingFirstCity -> return $
						[ Move (CitySource my_playername) (BuildFirstCityTarget my_playername coors) | coors <- _playerFirstCityCoors ]
					PlaceFirstFigures -> do
						possible_squares <- buildFigureCoors gamename playername Wagon (head _playerCityCoors)
						let possible_figures = [Wagon,Flag]
						return [ Move (FigureSource my_playername figure) (SquareTarget coors) | coors <- possible_squares, figure <- possible_figures ]
					GettingFirstTrade ->
						return [ Move (AutomaticMove ()) (GetTradeTarget my_playername) ]
					StartOfTurn -> do
						buildcitymovess <- case tokenStackAvailableKeys _playerCityStack of
							[] -> return []
							_ -> do
								forAllPlayerFigures gamename playername $ \ (figureid,Figure{..}) -> do
									canbuildcityhere <- canBuildCityHere gamename playername _figureCoors
									return $ case canbuildcityhere && (_figureType==Wagon) of
										False -> []
										True -> [ Move (FigureOnBoardSource figureid playername _figureCoors) (BuildCityTarget ()) ] 
						return $ concat buildcitymovess

					Trading ->
						return [ Move (AutomaticMove ()) (GetTradeTarget my_playername) ]
					CityManagement -> do
						movess <- forAllCities gamename my_playername $ \ (citycoors,city) -> do
							income <- cityIncome gamename my_playername citycoors

							possible_fig_coors <- buildFigureCoors gamename playername Wagon citycoors
							let
								possible_figures = map fst $ filter ((>0).snd) $ tokenStackHeights _playerFigures
								prodfiguremoves = [ Move (CityProductionSource citycoors (ProduceFigure figty)) (SquareTarget squarecoors) |
									figty <- filter ((<=income).consumedIncome) possible_figures,
									squarecoors <- possible_fig_coors ]

							let
								player_buildings = concatMap enabledBuildings $ playerAbilities player
								buildings_left = concatMap buildingMarkerToType $ map fst $ filter ((>0).snd) $ tokenStackHeights _gameBuildingStack
								all_avail_buildings = buildings_left `intersect` player_buildings
								avail_downgraded = [ dg | (_,[dg,ug]) <- buildingMarkerType, ug `elem` all_avail_buildings ]
								avail_buildings = filter (`notElem` avail_downgraded) all_avail_buildings
								affordable_buildings = filter ((<=income).consumedIncome) avail_buildings
							prodbuildingmovess <- forCityOutskirts gamename playername citycoors $ \ (coors,city,square) -> do
								let buildings = (terrainBuildings $ head (_squareTerrain square)) `intersect` affordable_buildings
								return [ Move (CityProductionSource citycoors (ProduceBuilding building)) (SquareTarget coors) |
									building <- buildings ]									

							produnitmovess <- forM (allOfThem::[UnitType]) $ \ unittype -> do
								let mb_unitlevel = getValueAbility1 unitLevel player unittype
								return $ case mb_unitlevel of
									Nothing -> []
									Just unitlevel -> case consumedIncome (unittype,unitlevel) <= income of
										False -> []
										True  -> [ Move (CityProductionSource citycoors (ProduceUnit unittype)) (NoTarget ()) ]

							let
								producible_res = case AnyResource `elem` inResource income of
									True -> [ Linen,Iron,Incense,Wheat ]
									False -> nub $ map oneResource $ inResource income
								avail_res = tokenStackAvailableKeys _gameResourceStack
								harvestmoves = map (\ res -> Move (CityProductionSource citycoors (HarvestResource res)) (NoTarget ())) $
									producible_res `intersect` avail_res

							let
								devotemove = 
									Move (CityProductionSource citycoors (DevoteToArts (inCulture income))) (NoTarget ())

							return $
								prodfiguremoves ++
								(concat prodbuildingmovess) ++
								(concat produnitmovess) ++
								harvestmoves ++
								[ devotemove ]

						return $ Move (AutomaticMove ()) (FinishPhaseTarget ()) : concat movess

					Movement -> do
						cmovess <- forAllPlayerFigures gamename playername $ \ (figureid,Figure{..}) -> do
							canstay <- canStayOn gamename playername _figureType _figureCoors
							case _figureRangeLeft of
								0 -> case canstay of
									True  -> return []
									False -> do
										destroyFigure gamename playername figureid
										return []
								_ -> do
									targetmustss <- forM (neighbourSquares _figureCoors) $ \ targetcoors -> do
										mb_square <- getSquare gamename targetcoors
										case mb_square of
											Nothing -> return []
											Just OutOfBounds -> return []
											Just (UnrevealedSquare tileid tileorigin) -> do
												let ori = coorDiffOri _figureCoors targetcoors
												return [ (RevealTileTarget ori tileorigin,False) ]
											Just _ -> do
												cancross_target <- canCross gamename playername _figureType targetcoors
												case cancross_target of
													False -> return []
													True -> do
														canstay_target <- canStayOn gamename playername _figureType targetcoors
														case canstay_target of
															False | _figureRangeLeft <=1 -> return []
															_ -> do
																return [(SquareTarget targetcoors,not canstay)]
									return [ (Move (FigureOnBoardSource figureid playername _figureCoors) target,mustmove) |
										(target,mustmove) <- concat targetmustss ]
						
						let
							cmoves = concat cmovess
							finishmoves = case all (not.snd) cmoves of
								True  -> [ Move (AutomaticMove ()) (FinishPhaseTarget ()) ]
								False -> []

						return $ finishmoves ++ map fst cmoves

					Research -> do
						let
							level_techs = (TechLevelI,[]) : map (\ level -> (level,maybe [] id (Map.lookup level _playerTechs))) allOfThem
							ptechs [_] = []
							ptechs ((_,t1):(l2,t2):ns) = ptechs ((l2,t2):ns) ++ case (length t1 > length t2 + 1) || l2==TechLevelI of
								False -> []
								True -> case (inTrade $ techCosts player l2) <= _playerTrade of
									False -> []
									True  -> techsOfLevel l2 \\ (map _techCardTechId $ concat $ Map.elems _playerTechs)
						return $ Move (AutomaticMove ()) (FinishPhaseTarget ()) :
							[ Move (TechSource tech) (TechTreeTarget playername) | tech <- ptechs level_techs ]

				abilitymovess <- forM (playerAbilities player) $ \ ability -> do
					resourcemovess <- forM (resourceAbilities ability _gamePhase) $ \ (tech,movename,resourcepats,hook) -> do
						return [ Move (ResourcesSource playername payment) (TechResourceAbilityTarget movename tech) | payment <- possiblePayments player resourcepats ]
					cardmovess <- forM (cardAbilities ability _gamePhase) $ \ (target,movename,hook) -> do
						return [ Move (NoSource ()) target ]
					return $ concat $ resourcemovess ++ cardmovess

				return $ phasemoves ++ concat abilitymovess

		mb_movesthisphase <- queryCivLensM $ civPlayerLens gamename playername . _Just . playerMoves . at _gameTurn . _Just . at _gamePhase . _Just
		let movesthisphase = maybe [] Prelude.id mb_movesthisphase
		return $ foldl (\ allowedmoves move1 -> filter (allowSecondMove move1) allowedmoves) moves movesthisphase
	case res of
		Right moves -> return moves
		Left msg -> error msg

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
	'autoPlayGame,
	'joinGame,
	'deleteGame,
	'createNewGame,
	'gameAction,
	'moveGen,
	'playerNumCoins
	])

$(deriveSafeCopy modelVersion 'base ''StdGen)

