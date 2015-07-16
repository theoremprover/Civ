module Acidic where

import Import hiding (Update,Query)

import qualified Prelude

import Data.Acid
import Data.Acid.Advanced
import Control.Monad.Error (throwError,runErrorT)
import Control.Lens hiding (Action)
import Control.Monad.State (modify,get,gets)
import Data.Maybe

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

queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ view lens civstate

$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'incTrade,
	'createNewGame
	])

takeFromStackU gamename stacklens toktyp = do
	updateCivLensU (takeFromStack toktyp) $ civGameLens gamename . stacklens

putOnStackU gamename stacklens toktyp tok = do
	updateCivLensU (putOnStack toktyp tok) $ civGameLens gamename . stacklens

squaresFromTile gamename tileid tilecoors orientation revealed = do
	forM (tileSquares tileid revealed) $ \ (tcoors,sq) -> do
		sq' <- case _squareTokenMarker sq of
			Just (HutMarker _) -> do
				hut <- takeFromStackU gamename gameHutStack ()
				return $ sq { _squareTokenMarker = Just (HutMarker hut) }
			Just (VillageMarker _) -> do
				village <- takeFromStackU gamename gameVillageStack ()
				return $ sq { _squareTokenMarker = Just (VillageMarker village) }
			_ -> return sq
		return (addCoors tilecoors (rotate4x4coors orientation tcoors),sq')

createBoard :: GameName -> Update CivState UpdateResult
createBoard gamename = do
	players <- queryCivLensH $ civPlayersLens gamename
	coorsquaress <- forM (boardLayout $ length players) $ \ (coors,layouttile) -> do
		(tileid,orientation,revealed) <- case layouttile of
			NT -> do
				tid <- takeFromStackU gamename gameTileStack ()
				return (tid,Nothing,False)
			CT playerindex ori -> do
				return (Tile $ _playerCiv (players Prelude.!! playerindex),Just ori,True)
		squaresFromTile gamename tileid coors orientation revealed

	let
		coorsquares = concat coorsquaress
		xcoorss = map (xCoor.fst) coorsquares
		ycoorss = map (yCoor.fst) coorsquares
		mincoors = Coors (minimum xcoorss) (minimum ycoorss)
		maxcoors = Coors (maximum xcoorss) (maximum ycoorss)
		gameboardarr = array (mincoors,maxcoors) coorsquares

	updateCivLensU (const gameboardarr) $ civGameLens gamename . _Just . gameBoard

