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
import System.Random

import qualified Data.ByteString.Lazy as L
import Data.Conduit.List (consume)

import Network.Wai (requestBody)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Model
import Entities
import Logic
import TokenStack
import Lenses
import Polls
import Acidic
import AssocList


queryCivLensH :: (MonadHandler m, HandlerSite m ~ App) => Traversal' CivState a -> m (Maybe a)
queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ preview lens civstate

updateCivH :: (UpdateEvent event,MethodState event ~ CivState,MethodResult event ~ UpdateResult) =>
	event -> Handler (EventResult event)
updateCivH event = do
	app <- getYesod
	update' (appCivAcid app) event
{-
	res <- update' (appCivAcid app) event
	when (isRight res) $ notifyLongPoll actiona affecteds
	return res
-}

queryCivH :: Traversal' CivState a -> Handler (Maybe a)
queryCivH lens = do
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

notifyLongPoll :: ActionA -> [Affected] -> Handler ()
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

pollHandler :: Handler ActionA
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
	bss <- rawRequestBody $$ consume
	let requestbody = BSL.fromChunks bss
	case decode requestbody :: Maybe ActionA of
		Nothing -> error $ "got: " ++ BSL.unpack requestbody
		Just action -> do
			res <- executeAction action
			sendResponse $ repJson $ encode res

getGameH gamename = queryCivLensH $ civGameLens gamename . _Just

maybeVisitor :: Handler (UserId,User,GameName,Game,Maybe PlayerName)
maybeVisitor = do
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing,_) -> errHandler "gamename not set in this session"
		Just (userid,user,Just gamename,mb_playername) -> do
			mb_game <- getGameH gamename
			case mb_game of
				Nothing -> errHandler $ "There is no game " ++ show gamename
				Just game -> return (userid,user,gamename,game,mb_playername)

executeAction :: ActionA -> Handler UpdateResult
executeAction action = do
	(userid,user) <- requireLoggedIn
	printLogDebug $ "executeAction " ++ show action
	case action of

		CreateGameA gamename@(GameName gn) -> do
			now <- liftIO $ getCurrentTime
			tilestack    <- shuffle initialBoardTileStack
			hutstack     <- shuffle initialHutStack
			villagestack <- shuffle initialVillageStack
			personstack  <- shuffle initialGreatPersonStack
			unitstack    <- shuffle initialUnitStack
			culturestack <- shuffle initialCultureStack
			res <- updateCivH $ CreateNewGame gamename $ Game
				now (userEmail user) Waiting emptyPlayers 1 StartOfGame 0 0
				emptyBoard
				tilestack hutstack villagestack
				initialBuildingStack personstack unitstack culturestack
				(initialResourceStack 0) False

			updateCivH $ JoinGame gamename (PlayerName "Red") (userEmail user) Red Russia
			updateCivH $ JoinGame gamename (PlayerName "Green") (userEmail user) Green Arabs
			updateCivH $ JoinGame gamename (PlayerName "Blue") (userEmail user) Blue America
			updateCivH $ JoinGame gamename (PlayerName "Yellow") (userEmail user) Yellow China
			setSession "game" gn
			setSession "player" "Red"

			notifyLongPoll action [GameAdmin]
			return res

		DeleteGameA gamename@(GameName gn) -> do
			res <- updateCivH $ DeleteGame gamename
			notifyLongPoll action [GameAdmin,GameGame gamename]
			return res

		JoinGameA gamename@(GameName gn) playername@(PlayerName pn) email colour civ -> do
			res <- updateCivH $ JoinGame gamename playername email colour civ
			case res of
				Right () -> do
					setSession "game" gn
					setSession "player" pn
				Left errmsg -> do
					deleteSession "game"
					deleteSession "player"
					setMessage $ toHtml errmsg
			notifyLongPoll action [GameAdmin,GameGame gamename]
			return res

		SetSessionGameA (GameName gn) -> do
			setSession "game" gn
			return oK

		SetSessionGamePlayerA (GameName gn) (PlayerName pn) -> do
			setSession "game" gn
			setSession "player" pn
			return oK

		StartGameA gamename autoplay -> do
			Just (AssocList playerlist) <- queryCivLensH (civPlayersLens gamename)
			shuffledplayers <- shuffleList playerlist
			updateCivH $ SetShuffledPlayers gamename $ AssocList shuffledplayers
			updateCivH $ StartGame gamename
			when autoplay $ do
				randgen <- liftIO $ getStdGen
				updateCivH $ AutoPlayGame gamename randgen
				return ()
			notifyLongPoll action [GameAdmin,GameGame gamename]
			return oK

		GameActionA move -> do
			(userid,user,gamename,game,mb_playername) <- maybeVisitor
			case mb_playername of
				Nothing -> return $ eRR $ show action ++ " cannot be given by visitors"
				Just playername -> do
					updateCivH $ GameAction gamename playername move
					notifyLongPoll action [GameGame gamename]
					return oK

		_ -> return $ eRR $ show action ++ " not implemented yet"
