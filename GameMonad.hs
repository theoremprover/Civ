{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,LambdaCase,Rank2Types #-}

module GameMonad where

import Data.Aeson
import Data.Aeson.TH

import Import hiding (Update,Query,get)

import qualified Prelude

import Model
import Entities

import Data.Acid
import Data.Acid.Advanced

import Control.Monad.Error (runErrorT,throwError)

import qualified Database.Persist.Sql as Sql

import Control.Lens hiding (Action)
import qualified Data.Map as Map
import Control.Monad.State (modify,get,gets)

import Data.Maybe
import Data.Either

import Polls

-------------- Actions

data Action =
	CreateGameA GameName |
	DeleteGameA GameName |
	JoinGameA GameName PlayerName Colour Civ |
	IncTradeA GameName PlayerName Trade
	deriving Show

------------- Splices

deriveJSON defaultOptions ''Action
deriveJSON defaultOptions ''Notification
deriveJSON defaultOptions ''Affected
deriveJSON defaultOptions ''Trade
deriveJSON defaultOptions ''GameName
deriveJSON defaultOptions ''PlayerName
deriveJSON defaultOptions ''Civ
deriveJSON defaultOptions ''Colour

-------- Credentials

errHandler :: String -> Handler a
errHandler msg = do
	sendResponse $ [hamlet|
<h1>Error
<h3 colour=red>#{msg}
|]

requireLoggedIn :: Handler (UserId,User)
requireLoggedIn = do
	Entity userid user <- requireAuth
	return (userid,user)

maybeVisitor :: Handler (UserId,User,GameName,Game,Maybe (PlayerName,Player))
maybeVisitor = do
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing,_) -> errHandler "gamename not set in this session"
		Just (userid,user,Just gamename,mb_playername) -> do
			mb_game <- getGame gamename
			case mb_game of
				Nothing -> do
					errHandler $ "There is no game " ++ show gamename
				Just game -> case mb_playername of
					Nothing -> return (userid,user,gamename,game,Nothing)
					Just playername -> do
						mb_gameplayer <- getGamePlayer gamename playername
						case mb_gameplayer of
							Nothing -> do
								errHandler $ "There is no player " ++ show playername ++ " in game " ++ show gamename
							Just (game,mb_player) -> return (userid,user,gamename,game,case mb_player of
								Nothing -> Nothing
								Just player -> Just (playername,player) )

------ Polling

getPollsMVar :: Handler Polls
getPollsMVar = getYesod >>= (return . appLongPolls)

waitLongPoll :: Affected -> Handler UpdateResult
waitLongPoll affected = do
	mvar <- liftIO $ newEmptyMVar 
	pollsmvar <- getPollsMVar
	liftIO $ modifyMVar_ pollsmvar $ return . ((affected,mvar):)
	printLogDebug $ "Waiting for Notification on " ++ show affected ++ " ..."
	Notification <- liftIO $ takeMVar mvar
	printLogDebug $ "Got Notification on " ++ show affected
	return oK

notifyLongPoll :: [Affected] -> Handler ()
notifyLongPoll affecteds = do
	pollsmvar <- getPollsMVar
	polls <- liftIO $ takeMVar pollsmvar
	forM_ (filter ((`elem` affecteds) . fst) polls) $ \ (ag,mvar) -> do
		printLogDebug $ "Notifying " ++ show ag
		liftIO $ putMVar mvar Notification
	let remainingpolls = filter ((`notElem` affecteds) . fst) polls
	liftIO $ putMVar pollsmvar remainingpolls
	printLogDebug $ "PutMVar remaining polls: " ++ show (map fst remainingpolls)
	return ()

pollHandler :: Handler ()
pollHandler = do
	(userid,user) <- requireLoggedIn
	affected :: Affected <- requireJsonBody
	waitLongPoll affected
	return ()

-------- Lenses

civStateLens = id
civGameLens gamename = civStateLens . civGames . at gamename

civPlayerLens gamename playername =
	civPlayersLens gamename . at playername

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

createNewGame :: GameName -> UserName -> Update CivState UpdateResult
createNewGame gamename username = runErrorT $ do
	checkCondition ("Cannot create " ++ show gamename ++ ": it already exists!")
		(civGameLens gamename . _Just) isNothing
	updateCivLensU (\_-> Just $ newGame username) $ civGameLens gamename

deleteGame :: GameName -> Update CivState UpdateResult
deleteGame gamename = runErrorT $ do
	updateCivLensU (\_-> Nothing) $ civGameLens gamename

joinGame :: GameName -> PlayerName -> Colour -> Civ -> Update CivState UpdateResult
joinGame gamename playername colour civ = runErrorT $ do
	checkCondition (show playername ++ " already exists in " ++ show gamename)
		(civPlayerLens gamename playername . _Just) isNothing
	checkCondition (show colour ++ " already taken in " ++ show gamename)
		(civGameLens gamename . _Just . gamePlayers) ((notElem colour) . (map _playerColour) . Map.elems . fromJust)
	updateCivLensU (\_ -> Just $ makePlayer colour civ) $
		civGameLens gamename . _Just . gamePlayers . at playername

startGame :: GameName -> Update CivState UpdateResult
startGame gamename = runErrorT $ do
	checkCondition ("Cannot start " ++ show gamename ++ " is not in waiting state.")
		(civGameLens gamename . _Just . gameState) (==(Just Waiting))
	updateCivLensU (\_ -> Running) $ civGameLens gamename . _Just . gameState

incTrade :: GameName -> PlayerName -> Trade -> Update CivState UpdateResult
incTrade gamename playername trade = runErrorT $ do
	updateCivLensU (+trade) $ civPlayerLens gamename playername . _Just . playerTrade


-------------- In Handler Monad

getCivState :: Query CivState CivState
getCivState = ask

getGamePlayer :: GameName -> PlayerName -> Handler (Maybe (Game,Maybe Player))
getGamePlayer gamename playername = do
	mb_game <- getGame gamename
	return $ case mb_game of
		Nothing -> Nothing
		Just game -> Just (game,Map.lookup playername (_gamePlayers game))

updateCivH affectedgamess event = do
	app <- getYesod
	res <- update' (appCivAcid app) event
	when (isRight res) $ notifyLongPoll affectedgamess
	return res

postCommandR :: Handler ()
postCommandR = do
	(userid,user) <- requireLoggedIn
	action :: Action <- requireJsonBody
 	res <- executeAction action
	sendResponse $ repJson $ encode res

getGame gamename = queryCivLensH $ civGameLens gamename

$(makeAcidic ''CivState [
	'getCivState,
	'startGame,
	'joinGame,
	'deleteGame,
	'incTrade,
	'createNewGame
	])

queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ view lens civstate

executeAction :: Action -> Handler UpdateResult
executeAction action = do
	(_,user) <- requireLoggedIn
	printLogDebug $ "executeAction " ++ show action
	case action of
		CreateGameA gamename ->
			updateCivH [GameAdmin] $ CreateNewGame gamename (userEmail user)
		DeleteGameA gamename ->
			updateCivH [GameAdmin,GameGame gamename] $ DeleteGame gamename
		JoinGameA gamename playername colour civ ->
			updateCivH [GameAdmin,GameGame gamename] $ JoinGame gamename playername colour civ
		IncTradeA gamename playername trade ->
			updateCivH [GameGame gamename] $ IncTrade gamename playername trade

		_ -> return $ eRR $ show action ++ " not implemented yet"


{-

queryCivLensU lens = gets lens

--------------

createNewGame :: GameName -> UserName -> Update CivState UpdateResult
createNewGame gamename username = runErrorT $ do
	checkCondition ("Cannot create " ++ show gamename ++ ": it already exists!")
		(civGameLens gamename . _Just) isNothing
	updateCivLensU (\_-> Just $ newGame username) $ civGameLens gamename

$(makeAcidic ''CivState [
	'getCivState,
	'createNewGame,
	])

data GameAdminAction =
	LongPollGAA |
	CreateGameGAA GameName |
	JoinGameGAA GameName PlayerName Colour Civ |
	PlayGameGAA GameName PlayerName |
	VisitGameGAA GameName |
	DeleteGameGAA GameName
	deriving Show
deriveJSON defaultOptions ''GameAdminAction

data WaitingAction =
	LongPollWA |
	StartGameWA GameName |
	CreatePlayerWA GameName
	deriving Show
deriveJSON defaultOptions ''WaitingAction	

data GameAction =
	LongPollGA |
	IncTradeGA Trade
	deriving Show
deriveJSON defaultOptions ''Trade
deriveJSON defaultOptions ''GameAction

deriveJSON defaultOptions ''GameName
deriveJSON defaultOptions ''PlayerName
deriveJSON defaultOptions ''Civ
deriveJSON defaultOptions ''Colour

----


------------

errRedirect :: String -> Handler a
errRedirect s = do
	setMessage $ toHtml s
	redirect HomeR

requirePlayerUserSessionCredentials :: Handler (UserId,User,GameName,Game,PlayerName,Player)
requirePlayerUserSessionCredentials = do
	(userid,user,gamename,game,mb_player) <- maybeVisitorUserSessionCredentials
	case mb_player of
		Nothing -> errRedirect "You are not a player in this game"
		Just (playername,player) -> return (userid,user,gamename,game,playername,player)
-}