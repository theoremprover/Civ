{-# LANGUAGE TemplateHaskell,ScopedTypeVariables,LambdaCase,Rank2Types #-}

module GameMonad where

import Data.Aeson.TH

import Import (
	Handler,Entity(..),requireAuth,getYesod,App(..),
	setSession,deleteSession,printLogDebug,
	AffectedGames(..),Notification(..),Polls,runDB)

import Prelude

import Model
import Entities

import Data.Acid
import Data.Acid.Advanced

import Control.Concurrent.MVar

import Control.Monad.Error

import qualified Database.Persist.Sql as Sql

import Control.Lens
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe
import Data.Either

-- Lenses

civStateLens = id
civGameLens gamename = civStateLens . civGames . at gamename

civPlayerLens gamename playername =
	civPlayersLens gamename . at playername

civPlayersLens gamename = civGameLens gamename . _Just . gamePlayers

-- In Update monad

updateCivLensU fval lens = do
	modify (over lens fval)
	return ()

queryCivLensU lens = gets lens

-------------- Conditions

checkCondition errmsg lens f = do
	civstate <- get
	case f (preview lens civstate) of
		False -> throwError errmsg
		True -> return ()

--------------

oK = Right ()
eRR errmsg = Left errmsg

type UpdateResult = Either String ()

getCivState :: Query CivState CivState
getCivState = ask

incTrade :: GameName -> PlayerName -> Trade -> Update CivState UpdateResult
incTrade gamename playername trade = runErrorT $ do
	updateCivLensU (+trade) $ civPlayerLens gamename playername . _Just . playerTrade

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

$(makeAcidic ''CivState [
	'getCivState,
	'createNewGame,
	'joinGame,
	'deleteGame,
	'incTrade
	])

data GameAdminAction =
	LongPollGAA |
	CreateGameGAA GameName |
	CreatePlayerGAA GameName |
	JoinGameGAA GameName PlayerName Colour Civ |
	PlayGameGAA GameName PlayerName |
	VisitGameGAA GameName |
	StartGameGAA GameName |
	DeleteGameGAA GameName
	deriving Show
deriveJSON defaultOptions ''GameAdminAction

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

getGame gamename = queryCivLensH $ civGameLens gamename

getGamePlayer :: GameName -> PlayerName -> Handler (Maybe (Game,Maybe Player))
getGamePlayer gamename playername = do
	mb_game <- getGame gamename
	return $ case mb_game of
		Nothing -> Nothing
		Just game -> Just (game,Map.lookup playername (_gamePlayers game))

requireLoggedIn :: Handler UserName
requireLoggedIn = do
	Entity userid user <- requireAuth
	return $ userEmail user

executeGameAdminAction :: GameAdminAction -> Handler UpdateResult
executeGameAdminAction gaa = do
	user <- requireLoggedIn
	case gaa of
		LongPollGAA -> do
			waitLongPoll GameAdmin
		CreateGameGAA gamename -> do
			updateCivH [GameAdmin] $ CreateNewGame gamename user
		JoinGameGAA gamename@(GameName gn) playername@(PlayerName pn) colour civ -> do
			Entity userid userentity <- requireAuth
			runDB $ Sql.update userid
				[ UserParticipations Sql.=. Map.insertWith' (++) gn [pn] (userParticipations userentity) ]
			res <- updateCivH [GameAdmin,GameGame gamename] $ JoinGame gamename playername colour civ
			case res of
				Right _ -> do
					setSession "game" gn
					setSession "player" pn
				Left _ -> do
					deleteSession "game"
					deleteSession "player"
			return res
		PlayGameGAA (GameName gn) (PlayerName pn) -> do
			setSession "game" gn
			setSession "player" pn
			return oK
		CreatePlayerGAA _ -> return oK
		VisitGameGAA gamename@(GameName gn) -> do
			setSession "game" gn
			deleteSession "player"
			return oK
		StartGameGAA gamename -> do
			return oK
		DeleteGameGAA gamename@(GameName gn) -> do
			Entity userid userentity <- requireAuth
			runDB $ Sql.update userid
				[ UserParticipations Sql.=. Map.delete gn (userParticipations userentity) ]
			updateCivH [GameAdmin,GameGame gamename] $ DeleteGame gamename

executeGameAction :: GameName -> PlayerName -> GameAction -> Handler UpdateResult
executeGameAction gamename playername gameaction = do
	case gameaction of
		LongPollGA -> waitLongPoll $ GameGame gamename
		IncTradeGA trade -> updateCivH [GameGame gamename] $ IncTrade gamename playername trade

----

-- In Handler monad

getPollsMVar :: Handler Polls
getPollsMVar = getYesod >>= (return . appLongPolls)

waitLongPoll :: AffectedGames -> Handler UpdateResult
waitLongPoll affectedgames = do
	mvar <- liftIO $ newEmptyMVar 
	pollsmvar <- getPollsMVar
	liftIO $ modifyMVar_ pollsmvar $ return . ((affectedgames,mvar):)
	printLogDebug $ "Waiting for Notification on " ++ show affectedgames ++ " ..."
	Notification <- liftIO $ takeMVar mvar
	printLogDebug $ "Got Notification on " ++ show affectedgames
	return oK

notifyLongPoll :: [AffectedGames] -> Handler ()
notifyLongPoll affectedgamess = do
	pollsmvar <- getPollsMVar
	polls <- liftIO $ takeMVar pollsmvar
	forM_ (filter ((`elem` affectedgamess) . fst) polls) $ \ (ag,mvar) -> do
		printLogDebug $ "Notifying " ++ show ag
		liftIO $ putMVar mvar Notification
	let remainingpolls = filter ((`notElem` affectedgamess) . fst) polls
	liftIO $ putMVar pollsmvar remainingpolls
	printLogDebug $ "PutMVar remaining polls: " ++ show (map fst remainingpolls)
	return ()

{-
updateCivH :: (UpdateEvent event,MethodState event ~ CivState, MethodResult event ~ (Maybe String)) =>
	[AffectedGames] -> event -> Handler (Maybe String) 
-}
updateCivH affectedgamess event = do
	app <- getYesod
	res <- update' (appCivAcid app) event
	when (isRight res) $ notifyLongPoll affectedgamess
	return res

queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ view lens civstate
