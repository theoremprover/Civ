{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,LambdaCase,Rank2Types #-}

module GameMonad where

import Data.Aeson
import Data.Aeson.TH

import Import hiding (Update,Query,get)

import qualified Prelude

import Model
import Entities

import Logic

import Data.Acid
import Data.Acid.Advanced

import Control.Monad.Error (runErrorT,throwError)

--import qualified Database.Persist.Sql as Sql

import Control.Lens hiding (Action)
import qualified Data.Map as Map
import Control.Monad.State (modify,get,gets)

import Data.Maybe
import Data.Either

import qualified Data.Text as Text

import Polls

------------- Splices

deriveJSON defaultOptions ''Action
deriveJSON defaultOptions ''Affected
deriveJSON defaultOptions ''Trade
deriveJSON defaultOptions ''GameName
deriveJSON defaultOptions ''PlayerName
deriveJSON defaultOptions ''Civ
deriveJSON defaultOptions ''Colour

--------- Errors

errHandler :: String -> Handler a
errHandler msg = invalidArgs [Text.pack msg]

errHamlet msg = [whamlet|
<h1>Error
<h3>#{msg}
|]

-------- Credentials

requireLoggedIn :: Handler (UserId,User)
requireLoggedIn = do
	Entity userid user <- requireAuth
	return (userid,user)

------ Polling

getPollsMVar :: Handler Polls
getPollsMVar = getYesod >>= (return . appLongPolls)

notifyLongPoll :: Action -> [Affected] -> Handler ()
notifyLongPoll action affecteds = do
	pollsmvar <- getPollsMVar
	polls <- liftIO $ takeMVar pollsmvar
	forM_ (filter ((`elem` affecteds) . fst) polls) $ \ (ag,mvar) -> do
		printLogDebug $ "Notifying " ++ show ag
		liftIO $ putMVar mvar action
	let remainingpolls = filter ((`notElem` affecteds) . fst) polls
	liftIO $ putMVar pollsmvar remainingpolls
	printLogDebug $ "PutMVar remaining polls: " ++ show (map fst remainingpolls)
	return ()

pollHandler :: Handler Action
pollHandler = do
	(userid,user) <- requireLoggedIn
	affected :: Affected <- requireJsonBody
	mvar <- liftIO $ newEmptyMVar 
	pollsmvar <- getPollsMVar
	liftIO $ modifyMVar_ pollsmvar $ return . ((affected,mvar):)
	printLogDebug $ "Waiting for Notification on " ++ show affected ++ " ..."
	action <- liftIO $ takeMVar mvar
	printLogDebug $ "Got Notification on " ++ show affected
	return action

-------- Lenses

assocListLens key = lens (Prelude.lookup key) setter where
	setter list Nothing = filter ((==key).fst) list
	setter [] (Just val) = [(key,val)]
	setter ((k,a):kas) (Just val) | k==key = (k,val) : kas
	setter (ka:kas) jval = ka : setter kas jval

civStateLens = id
civGameLens gamename = civStateLens . civGames . at gamename

civPlayerLens gamename playername =
	civPlayersLens gamename . assocListLens playername

civPlayersLens gamename = civGameLens gamename . _Just . gamePlayers


-------- In Update/Query monad



updateCivLensU fval lens = do
	modify (over lens fval)
	return ()

oK = Right ()
eRR errmsg = Left errmsg

type UpdateResult = Either String ()

-------------- Conditions

checkCondition errmsg lens f = do
	civstate <- get
	case f (preview lens civstate) of
		False -> throwError errmsg
		True -> return ()

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

takeFromStackU gamename stacklens toktyp = do
	updateCivLensU (takeFromStack toktyp) $ civGameLens gamename . stacklens

putOnStackU gamename stacklens toktyp tok = do
	updateCivLensU (putOnStack toktyp tok) $ civGameLens gamename . stacklens

squaresFromTile tileid tilecoors orientation revealed = do
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
	players <- queryCivLensH $ civGamePlayers gamename
	coorsquaress <- forM (boardLayout $ length players) $ \ (coors,layouttile) -> do
		(tileid,orientation,revealed) <- case layouttile of
			NT -> do
				tid <- takeFromStackU gamename boardTileStack ()
				return (tid,Nothing,False)
			CT playerindex ori -> do
				return (Tile $ _playerCiv (players!!playerindex),Just ori,True)
		squaresFromTile tileid coors orientation revealed

	let
		coorsquares = concat coorsquaress
		xcoorss = map (xCoor.fst) coorsquares
		ycoorss = map (yCoor.fst) coorsquares
		mincoors = Coors (minimum xcoorss) (minimum ycoorss)
		maxcoors = Coors (maximum xcoorss) (maximum ycoorss)
		gameboardarr = array (mincoors,maxcoors) coorsquares

	updateCivLensU (const gameboardarr) $ civGameLens gamename . _Just . gameBoard


-------------- In Handler Monad

getCivState :: Query CivState CivState
getCivState = ask

updateCivH action affectedgamess event = do
	app <- getYesod
	res <- update' (appCivAcid app) event
	when (isRight res) $ notifyLongPoll action affectedgamess
	return res

$(makeAcidic ''CivState [
	'getCivState,
	'setShuffledPlayers,
	'startGame,
	'joinGame,
	'deleteGame,
	'incTrade,
	'createNewGame
	])

getGamePlayer :: GameName -> PlayerName -> Handler (Maybe (Game,Maybe Player))
getGamePlayer gamename playername = do
	mb_game <- getGame gamename
	return $ case mb_game of
		Nothing -> Nothing
		Just game -> Just (game,Prelude.lookup playername (_gamePlayers game))

postCommandR :: Handler ()
postCommandR = do
	(userid,user) <- requireLoggedIn
	action :: Action <- requireJsonBody
 	res <- executeAction action
	sendResponse $ repJson $ encode res

getGame gamename = queryCivLensH $ civGameLens gamename

queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ view lens civstate

maybeVisitor :: Handler (UserId,User,GameName,Game,Maybe PlayerName)
maybeVisitor = do
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing,_) -> errHandler "gamename not set in this session"
		Just (userid,user,Just gamename,mb_playername) -> do
			mb_game <- getGame gamename
			case mb_game of
				Nothing -> errHandler $ "There is no game " ++ show gamename
				Just game -> return (userid,user,gamename,game,mb_playername)

executeAction :: Action -> Handler UpdateResult
executeAction action = do
	(userid,user) <- requireLoggedIn
	printLogDebug $ "executeAction " ++ show action
	case action of

		CreateGameA gamename -> do
			now <- liftIO $ getCurrentTime
			hutstack     <- shuffle initialHutStack
			villagestack <- shuffle initialVillageStack
			tilestack    <- shuffle initialBoardTileStack
			updateCivH action [GameAdmin] $ CreateNewGame gamename $
				Game (userEmail user) now Waiting [] [] tilestack hutstack villagestack

		DeleteGameA gamename@(GameName gn) -> do
			updateCivH action [GameAdmin,GameGame gamename] $ DeleteGame gamename

		JoinGameA gamename@(GameName gn) playername@(PlayerName pn) email colour civ -> do
			res <- updateCivH action [GameAdmin,GameGame gamename] $ JoinGame gamename playername email colour civ
			case res of
				Right () -> do
					setSession "player" pn
				Left errmsg -> do
					deleteSession "game"
					deleteSession "player"
					setMessage $ toHtml errmsg
			return res

		SetSessionGameA (GameName gn) -> do
			setSession "game" gn
			return oK

		SetSessionGamePlayerA (GameName gn) (PlayerName pn) -> do
			setSession "game" gn
			setSession "player" pn
			return oK

		StartGameA gamename -> do
			shuffledplayers <- queryCivLensH (civPlayersLens gamename) >>= shuffleList
			updateCivH action [] $ SetShuffledPlayers gamename shuffledplayers
			updateCivH action [GameAdmin,GameGame gamename] $ StartGame gamename

		IncTradeA gamename playername trade -> do
			updateCivH action [GameGame gamename] $ IncTrade gamename playername trade

		_ -> return $ eRR $ show action ++ " not implemented yet"

------------

