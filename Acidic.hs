{-# LANGUAGE RankNTypes #-}

module Acidic where

import Import hiding (Update,Query)

import qualified Prelude

import Data.Acid
import Data.Acid.Advanced
import Control.Monad.Error (throwError,runErrorT)
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

createNewGame :: GameName -> Game -> Update CivState UpdateResult
createNewGame gamename game = runErrorT $ do
	checkCondition ("Cannot create " ++ show gamename ++ ": it already exists!")
		(civGameLens gamename . _Just) isNothing
	updateCivLensU (\_-> Just $ game) $ civGameLens gamename

deleteGame :: GameName -> Update CivState UpdateResult
deleteGame gamename = runErrorT $ do
	updateCivLensU (\_-> Nothing) $ civGameLens gamename

joinGame :: GameName -> PlayerName -> PlayerEmail -> Colour -> Civ -> Update CivState UpdateResult
joinGame gamename playername email colour civ = runErrorT $ do
	checkCondition (show playername ++ " already exists in " ++ show gamename)
		(civPlayerLens gamename playername . _Just) isNothing
	checkCondition (show colour ++ " already taken in " ++ show gamename)
		(civGameLens gamename . _Just . gamePlayers) ((notElem colour) . (map (_playerColour.snd)) . fromJust)
	updateCivLensU (\_ -> Just $ makePlayer email colour civ) $
		civGameLens gamename . _Just . gamePlayers . assocListLens playername

startGame :: GameName -> Update CivState UpdateResult
startGame gamename = runErrorT $ do
	checkCondition ("Cannot start " ++ show gamename ++ " is not in waiting state.")
		(civGameLens gamename . _Just . gameState) (==(Just Waiting))
	updateCivLensU (const Running) $ civGameLens gamename . _Just . gameState
	createBoard gamename

incTrade :: GameName -> PlayerName -> Trade -> Update CivState UpdateResult
incTrade gamename playername trade = runErrorT $ do
	updateCivLensU (+trade) $ civPlayerLens gamename playername . _Just . playerTrade

setShuffledPlayers :: GameName -> Update CivState UpdateResult
setShuffledPlayers gamename players = do
	updateCivLensU (const players) $ civPlayersLens gamename

createBoard :: GameName -> Update CivState ()
createBoard gamename = do
	players <- queryCivLensU $ civPlayersLens gamename
	coorsquaress <- forM (boardLayout $ length players) $ \ (coors,layouttile) -> do
		(tileid,mb_orientation,revealed) <- case layouttile of
			NT -> do
				tid <- takeFromStackU gamename gameTileStack ()
				return (tid,Nothing,False)
			CT playerindex ori -> do
				return (Tile $ _playerCiv (players Prelude.!! playerindex),Just ori,True)
		squaresFromTile gamename tileid coors mb_orientation revealed

	let
		coorsquares = concat coorsquaress
		xcoorss = Prelude.map (xCoor.fst) coorsquares
		ycoorss = Prelude.map (yCoor.fst) coorsquares
		mincoors = Coors (Prelude.minimum xcoorss) (Prelude.minimum ycoorss)
		maxcoors = Coors (Prelude.maximum xcoorss) (Prelude.maximum ycoorss)
		gameboardarr = Array.array (mincoors,maxcoors) coorsquares
	updateCivLensU (const gameboardarr) $ civGameLens gamename . _Just . gameBoard
	return ()

--takeFromStackU :: (Ord toktyp) => Lens' CivState (TokenStack toktyp tok) -> toktyp -> Update CivState (Maybe tok)
takeFromStackU stacklens toktyp = do
	stack <- queryCivLensU stacklens
	case takeFromStack toktyp stack of
		Nothing -> return Nothing
		Just (tok,stack') -> do
			updateCivLensU (\_-> stack') stacklens
			return $ Just tok

--putOnStackU :: (Ord toktyp) => Lens' CivState (TokenStack toktyp tok) -> toktyp -> tok -> Update CivState ()
putOnStackU stacklens toktyp tok = do
	updateCivLensU (putOnStack toktyp tok) stacklens

squaresFromTile :: GameName -> TileID -> Coors -> Maybe Orientation -> Bool -> Update CivState [(Coors,Square)]
squaresFromTile _ tileid tilecoors Nothing False = do
	forM (tileSquares tileid) $ \ (tcoors,_) -> do
		return (tcoors,UnrevealedSquare tileid tilecoors)
squaresFromTile gamename tileid tilecoors (Just orientation) revealed = do
	forM (tileSquares tileid) $ \ (tcoors,sq) -> do
		sq' <- case _squareTokenMarker sq of
			Just (HutMarker _) -> do
				mb_hut <- takeFromStackU (civGameLens gamename . _Just . gameHutStack) ()
				return $ sq { _squareTokenMarker = fmap HutMarker mb_hut }
			Just (VillageMarker _) -> do
				mb_village <- takeFromStackU (civGameLens gamename . _Just . gameVillageStack) ()
				return $ sq { _squareTokenMarker = fmap VillageMarker mb_village }
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

