{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}

module Acidic where

import Import hiding (Update,Query,array)

import qualified Prelude

import Data.Acid
import Data.Acid.Advanced
import Control.Monad.Error (throwError,runErrorT,ErrorT)
import Data.Maybe
import Control.Lens hiding (Action)
import Control.Monad.State (modify,get,gets)

import Data.Array.IArray (array,(//),assocs)
import Data.Ix

import Logic
import Lenses
import TokenStack
import Model

getCivState :: Query CivState CivState
getCivState = ask

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

	Just (pn0,p0) <- queryCivLensM $ civPlayerIndexLens gamename 0
	Just (pn1,p1) <- queryCivLensM $ civPlayerIndexLens gamename 1
	buildCity gamename (Coors 6 13) $ City pn0 True False False NoWalls False (Just Southward)
	buildCity gamename (Coors 6 10) $ City pn0 False False False Walls False Nothing
	buildCity gamename (Coors 2 1) $ City pn1 True False False Walls False (Just Eastward)
	buildCity gamename (Coors 2 4) $ City pn1 True False False NoWalls False (Just Eastward)
	buildCity gamename (Coors 5 1) $ City pn1 False False False NoWalls False Nothing

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
				revealTile gamename coors Northward
			CT playerindex ori -> do
				Just (playername,player) <- queryCivLensM $ civPlayerIndexLens gamename playerindex
				updateBoard gamename $ squaresfromtile (Tile $ _playerCiv player) coors
				updateCivLensM (const ori) $ civPlayerLens gamename playername . _Just . playerOrientation
				revealTile gamename coors ori

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

type UpdateCivM a = ErrorT String (Update CivState) a

type UpdateResult = Either String ()

oK = Right ()
eRR errmsg = Left errmsg

runUpdateCivM :: UpdateCivM () -> Update CivState UpdateResult
runUpdateCivM = runErrorT

updateCivLensM :: (val -> val) -> Traversal' CivState val -> UpdateCivM () 
updateCivLensM fval lens = modify $ over lens fval

queryCivLensM :: Traversal' CivState a -> UpdateCivM (Maybe a)
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
buildCity :: GameName -> Coors -> City -> UpdateCivM ()
buildCity gamename coors city@(City{..}) = do
	let coorss = case _cityMetropolisOrientation of
		Nothing  -> [ coors ]
		Just ori -> [ coors, addCoorsOri coors ori ]
	let outskirts = outskirtsOf coorss

	Just () <- takeFromStackM (civPlayerLens gamename _cityOwner . _Just . playerCityStack) ()
	updateCivLensM (const $ Just $ CityMarker city) $ civSquareLens gamename coors . squareTokenMarker
	case _cityMetropolisOrientation of
		Nothing  -> return ()
		Just ori -> updateCivLensM (const $ Just $ CityMarker $ SecondCitySquare ori) $
			civSquareLens gamename (addCoorsOri coors ori) . squareTokenMarker

$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'createNewGame
	])

