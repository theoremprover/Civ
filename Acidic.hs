{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}

module Acidic where

import Import hiding (Update,Query,array,delete)

import qualified Prelude

import Data.Acid
import Data.Acid.Advanced
import Control.Monad.Error (throwError,runErrorT,ErrorT)
import Data.Maybe
import Control.Lens hiding (Action)
import Control.Monad.State (modify,get,gets,MonadState)
import Data.List (delete)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)

import Data.Array.IArray (array,(//),assocs)
import Data.Ix

import Logic
import Lenses
import TokenStack
import Model
import Polls
import ModelVersion

getCivState :: Query CivState CivState
getCivState = ask

type UpdateCivM a = ErrorT String (Update CivState) a
--type CheckPossibilityM = 

{-
type QueryCivM a = ErrorT String (Query CivState) a

queryCivQ :: Traversal' CivState a -> QueryCivM a
queryCivQ lens = do
	civstate <- Control.Monad.Reader.ask
	return $ preview lens civstate
-}

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

getNumPlayers gamename = do
	Just players <- queryCivLensM $ civPlayersLens gamename
	return $ numPlayers players

incPlayerIndex :: GameName -> Lens' Game Int -> UpdateCivM ()
incPlayerIndex gamename playerindex = do
	numplayers <- getNumPlayers gamename
	updateCivLensM ((`mod` numplayers) . (+1)) $ civGameLens gamename . _Just . playerindex

finishPlayerPhase gamename = do
	incPlayerIndex gamename gamePlayersTurn
	Just (game@Game{..}) <- queryCivLensM $ civGameLens gamename . _Just
	when (_gamePlayersTurn == _gameStartPlayer) $ do
		updateCivLensM nextPhase $ civGameLens gamename . _Just . gamePhase
		Just phase <- queryCivLensM $ civGameLens gamename . _Just . gamePhase
		when (phase==StartOfTurn) $ do
			incPlayerIndex gamename gamePlayersTurn
			incPlayerIndex gamename gameStartPlayer

gameAction :: GameName -> PlayerName -> Move -> Update CivState UpdateResult
gameAction gamename playername move@(Move source target) = runUpdateCivM $ do
	Just game <- queryCivLensM $ civGameLens gamename . _Just
	moves <- moveGen gamename (Just playername)
	case move `elem` moves of
		False -> error $ show playername ++ " requested " ++ show move ++ " which is not in 'moves'!"
		True -> case (source,target) of
			(CitySource pn1,BuildFirstCityTarget pn2 coors) | pn1==playername && pn2==playername -> do
				buildCity gamename coors $ newCity playername True Nothing
			(AutomaticMove (),GetTrade pn) | pn==playername -> getTrade gamename playername
			_ -> error $ show move ++ " not implemented yet"
	checkMovesLeft gamename

checkMovesLeft :: GameName -> Update CivState UpdateResult
checkMovesLeft gamename = do
	Just playername <- queryCivLensM $ civGameLens gamename . _Just
	moves_left <- moveGen gamename (Just playername)
	case moves_left of
		[] -> do
			finishPlayerPhase gamename
			

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
getSquare gamename coors = queryCivLensM $ civSquareLens gamename coors

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

{-
data Square =
	OutOfBounds |
	UnrevealedSquare TileID Coors |
	Square {
		_squareTileIDOri   :: Maybe (TileID,Orientation),
		_squareTerrain     :: [Terrain],
		_squareCoin        :: Bool,
		_squareResource    :: Maybe Resource,
		_squareNatWonder   :: Bool,
		_squareTokenMarker :: Maybe TokenMarker,
		_squareBuilding    :: Maybe Building,
		_squareFigures     :: [(Figure,PlayerName)]
		}
-}

{-
buildCityPossible :: GameName -> Coors -> City -> QueryCivM ()
buildCityPossible gamename coors city@(City{..}) = do
	let coorss = case _cityMetropolisOrientation of
		Nothing  -> [ coors ]
		Just ori -> [ coors, addCoorsOri coors ori ]
	let outskirts = outskirtsOf coorss
-}

buildCity :: GameName -> Coors -> City -> UpdateCivM ()
buildCity gamename coors city@(City{..}) = do
	Just () <- takeFromStackM (civPlayerLens gamename _cityOwner . _Just . playerCityStack) ()
	updateCivLensM (const $ Just $ CityMarker city) $ civSquareLens gamename coors . squareTokenMarker
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

$(deriveSafeCopy modelVersion 'base ''ActionSource)
$(deriveSafeCopy modelVersion 'base ''ActionTarget)
$(deriveSafeCopy modelVersion 'base ''Move)

$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'createNewGame,
	'gameAction
	])

