{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,LambdaCase,Rank2Types #-}

module GameMonad where

import Import
import qualified Prelude

import Data.Aeson
import Data.Text as Text (pack) 
import Control.Lens hiding (Action)
import Data.Acid
import Data.Acid.Advanced
import Data.Either

import Model
import Entities
import Logic
import TokenStack
import Lenses
import Polls
import Acidic


updateCivH :: (UpdateEvent event,MethodState event ~ CivState,MethodResult event ~ UpdateResult) =>
	Action -> [Affected] -> event -> Handler (EventResult event)
updateCivH action affecteds event = do
	app <- getYesod
	res <- update' (appCivAcid app) event
	when (isRight res) $ notifyLongPoll action affecteds
	return res

--queryCivLensH :: Traversal' CivState a -> Handler (Maybe a)
queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ preview lens civstate

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

postCommandR :: Handler ()
postCommandR = do
	(userid,user) <- requireLoggedIn
	action :: Action <- requireJsonBody
 	res <- executeAction action
	sendResponse $ repJson $ encode res

getGame gamename = queryCivLensH $ civGameLens gamename . _Just

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
			tilestack    <- shuffle initialBoardTileStack
			hutstack     <- shuffle initialHutStack
			villagestack <- shuffle initialVillageStack
			personstack  <- shuffle initialGreatPersonStack
			unitstack    <- shuffle initialUnitStack
			updateCivH action [GameAdmin] $ CreateNewGame gamename $ Game
				now (userEmail user) Waiting [] emptyPlayers 1 0
				emptyBoard
				tilestack hutstack villagestack
				initialBuildingStack personstack unitstack 

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
			Just (AssocList playerlist) <- queryCivLensH (civPlayersLens gamename)
			shuffledplayers <- shuffleList playerlist
			updateCivH action [] $ SetShuffledPlayers gamename $ AssocList shuffledplayers
			updateCivH action [GameAdmin,GameGame gamename] $ StartGame gamename

		IncTradeA gamename playername trade -> do
			updateCivH action [GameGame gamename] $ IncTrade gamename playername trade

		_ -> return $ eRR $ show action ++ " not implemented yet"
