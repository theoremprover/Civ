{-# LANGUAGE RankNTypes #-}

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
createNewGame gamename game = runErrorT $ do
	checkCondition ("Cannot create " ++ show gamename ++ ": it already exists!")
		(civGameLens gamename) isNothing
	updateCivLensM (\_-> Just $ game) $ civGameLens gamename

deleteGame :: GameName -> Update CivState UpdateResult
deleteGame gamename = runErrorT $ do
	updateCivLensM (\_-> Nothing) $ civGameLens gamename

joinGame :: GameName -> PlayerName -> PlayerEmail -> Colour -> Civ -> Update CivState UpdateResult
joinGame gamename playername email colour civ = runErrorT $ do
	checkCondition (show playername ++ " already exists in " ++ show gamename)
		(civPlayerLens gamename playername) isNothing
	checkCondition (show colour ++ " already taken in " ++ show gamename)
		(civGameLens gamename . _Just . gamePlayers) ((notElem colour) . (map (_playerColour.snd)) . fromJust)
	updateCivLensM (\_ -> Just $ makePlayer email colour civ) $
		civGameLens gamename . _Just . gamePlayers . assocListLens playername

startGame :: GameName -> Update CivState UpdateResult
startGame gamename = runErrorT $ do
	checkCondition ("Cannot start " ++ show gamename ++ ", it is not in waiting state.")
		(civGameLens gamename . _Just . gameState) (==(Just Waiting))
	updateCivLensM (const Running) $ civGameLens gamename . _Just . gameState
	createBoard gamename

incTrade :: GameName -> PlayerName -> Trade -> Update CivState UpdateResult
incTrade gamename playername trade = runErrorT $ do
	updateCivLensM (+trade) $ civPlayerLens gamename playername . _Just . playerTrade

setShuffledPlayers :: GameName -> Players -> Update CivState UpdateResult
setShuffledPlayers gamename players = do
	updateCivLensM (const players) $ civPlayersLens gamename

createBoard :: GameName -> UpdateCivM ()
createBoard gamename = do
	players <- queryCivLensM $ civPlayersLens gamename
	coorsquaress <- forM (boardLayout $ length players) $ \ (coors,layouttile) -> do
		(tileid,mb_orientation,revealed) <- case layouttile of
			NT -> do
				Just tid <- takeFromStackU (civGameLens gamename . _Just . gameTileStack) ()
				return (tid,Nothing,False)
			CT playerindex ori -> do
				(playername,player) <- civPlayerIndexLens gamename playerindex
				return (Tile $ _playerCiv player,Just ori,True)
		squaresFromTile gamename tileid coors mb_orientation revealed

	let
		coorsquares = concat coorsquaress
		xcoorss = Prelude.map (xCoor.fst) coorsquares
		ycoorss = Prelude.map (yCoor.fst) coorsquares
		mincoors = Coors (Prelude.minimum xcoorss) (Prelude.minimum ycoorss)
		maxcoors = Coors (Prelude.maximum xcoorss) (Prelude.maximum ycoorss)
		gameboardarr = Array.array (mincoors,maxcoors) coorsquares
	updateCivLensM (const gameboardarr) $ civGameLens gamename . _Just . gameBoard

takeFromStackU :: (Ord toktyp) => Lens' CivState (TokenStack toktyp tok) -> toktyp -> UpdateCivM (Maybe tok)
takeFromStackU stacklens toktyp = do
	stack <- queryCivLensM stacklens
	case takeFromStack toktyp stack of
		Nothing -> return Nothing
		Just (tok,stack') -> do
			updateCivLensM (\_-> stack') stacklens
			return $ Just tok

putOnStackU :: (Ord toktyp) => Lens' CivState (TokenStack toktyp tok) -> toktyp -> tok -> UpdateCivM ()
putOnStackU stacklens toktyp tok = do
	updateCivLensM (putOnStack toktyp tok) stacklens

squaresFromTile :: GameName -> TileID -> Coors -> Maybe Orientation -> Bool -> UpdateCivM [(Coors,Square)]
squaresFromTile _ tileid tilecoors Nothing False = do
	forM (tileSquares tileid) $ \ (tcoors,_) -> do
		return (tcoors,UnrevealedSquare tileid tilecoors)
squaresFromTile gamename tileid tilecoors (Just orientation) revealed = do
	forM (tileSquares tileid) $ \ (tcoors,sq) -> do
		sq' <- case _squareTokenMarker sq of
			Just (HutMarker _) -> do
				Just hut <- takeFromStackU (civGameLens gamename . gameHutStack) ()
				return $ sq { _squareTokenMarker = fmap HutMarker hut }
			Just (VillageMarker _) -> do
				Just village <- takeFromStackU (civGameLens gamename . gameVillageStack) ()
				return $ sq { _squareTokenMarker = fmap VillageMarker village }
			_ -> return sq
		return (addCoors tilecoors (rotate4x4coors orientation tcoors),sq')
		
$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'incTrade,
	'createNewGame
	])

