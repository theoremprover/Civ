{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}

module Acidic where

import Import hiding (Update,Query)

import qualified Prelude

import Data.Acid
import Data.Acid.Advanced
import Control.Monad.Error (throwError,runErrorT,ErrorT)
import Control.Lens hiding (Action)
import Control.Monad.State (modify,get,gets)
import Data.Maybe

import qualified Data.Array.IArray as Array

import Logic
import Lenses
import TokenStack
import Model


type UpdateResult = Either String ()

oK = Right ()
eRR errmsg = Left errmsg

getCivState :: Query CivState CivState
getCivState = ask

type UpdateCivM a = ErrorT String (Update CivState) a

runUpdateCivM :: UpdateCivM () -> Update CivState UpdateResult
runUpdateCivM = runErrorT

updateCivLensM :: (val -> val) -> Traversal' CivState val -> UpdateCivM () 
updateCivLensM fval lens = modify $ over lens fval

queryCivLensM :: Traversal' CivState a -> UpdateCivM (Maybe a)
queryCivLensM lens = do
	civstate <- Control.Monad.State.get
	return $ preview lens civstate

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

incTrade :: GameName -> PlayerName -> Trade -> Update CivState UpdateResult
incTrade gamename playername trade = runUpdateCivM $ do
	updateCivLensM (+trade) $ civPlayerLens gamename playername . _Just . playerTrade

setShuffledPlayers :: GameName -> Players -> Update CivState UpdateResult
setShuffledPlayers gamename players = runUpdateCivM $ do
	updateCivLensM (const players) $ civPlayersLens gamename

updateBoard :: GameName -> [(Coors,Square)] -> UpdateCivM ()
updateBoard gamename coorsquares = do
	updateCivLensM (Array.// coorsquares) $ civGameLens gamename . _Just . gameBoard
	
createBoard :: GameName -> UpdateCivM ()
createBoard gamename = do
	Just players <- queryCivLensM $ civPlayersLens gamename
	let
		layout = boardLayout (numPlayers players)
		lcoors = map fst layout
		(lxcoors,lycoors) = (map xCoor lcoors,map yCoor lcoors)
		lower = Coors (Prelude.minimum lxcoors) (Prelude.minimum lycoors)
		upper = Coors (Prelude.maximum lxcoors) (Prelude.maximum lycoors)
	updateCivLensM (const $ array (lower,upper) []) $ civGameLens gamename . _Just . gameBoard
	forM_ layout $ \ (coors,layouttile) -> do
		case layouttile of
			NT -> do
				Just tid <- takeFromStackM (civGameLens gamename . _Just . gameTileStack) ()
				updateBoard gamename (squaresfromtile tid coors)
			CT playerindex ori -> do
				Just (playername,player) <- queryCivLensM $ civPlayerIndexLens gamename playerindex
				updateCivLensM (Array.// (squaresfromtile (Tile $ _playerCiv player) coors)) $
					civGameLens gamename . _Just . gameBoard
				revealTile gamename coors ori

	where
	squaresfromtile :: TileID -> Coors -> [(Coors,Square)]
	squaresfromtile tileid tilecoors = (flip map) (tileSquares tileid) $
		\ (tcoors,_) -> (tcoors,UnrevealedSquare tileid tilecoors)

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
	Just (UnrevealedSquare tileid tilecoors) <- getSquare gamename coors
	coorssquares <- forM (tileSquares tileid) $ \ (tcoors,sq) -> do
		sq' <- case _squareTokenMarker sq of
			Just (HutMarker _) -> do
				mb_hut <- takeFromStackM (civGameLens gamename . _Just . gameHutStack) ()
				return $ sq { _squareTokenMarker = fmap HutMarker mb_hut }
			Just (VillageMarker _) -> do
				mb_village :: Maybe Village <- takeFromStackM (civGameLens gamename . _Just . gameVillageStack) ()
				return $ sq { _squareTokenMarker = fmap VillageMarker mb_village }
			_ -> return sq
		let sqcoors = addCoors tilecoors (rotate4x4coors orientation tcoors)
		let tileidori = case tilecoors==sqcoors of
			True  -> Just (tileid,orientation)
			False -> Nothing
		return (sqcoors,sq' { _squareTileIDOri = tileidori } )
	updateBoard gamename coorssquares

getSquare :: GameName -> Coors -> UpdateCivM (Maybe Square)
getSquare gamename coors = queryCivLensM $
	civGameLens gamename . _Just . gameBoard . ix coors

$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'incTrade,
	'createNewGame
	])

