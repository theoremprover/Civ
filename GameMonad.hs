{-# LANGUAGE TemplateHaskell,ScopedTypeVariables #-}

module GameMonad where

import Data.Aeson.TH

import Import (Handler,Entity(..),requireAuth,getYesod,App(..))
import Prelude

import Model
import Entities

import Data.Acid
import Data.Acid.Advanced

import Control.Lens
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

-- Lenses

civStateLens = id
civGameLens gamename = civStateLens . civGames . (at gamename)

-- In Update monad

updateCivLensU val lens = do
	modify $ set lens val

--------------

getCivState :: Query CivState CivState
getCivState = ask

incTrade :: PlayerName -> GameName -> Trade -> Update CivState ()
incTrade playername gamename trade = do
	-- hier die Lens
	return ()

createNewGame :: GameName -> UserName -> Update CivState (Maybe String)
createNewGame gamename username = do
	civstate <- get
	case view (civGameLens gamename) civstate of
		Just _ -> return $ Just $ "Cannot create " ++ show gamename ++ ": it already exists!"
		Nothing -> do
			updateCivLensU (Just $ newGame username) $ civGameLens gamename
			return Nothing

deleteGame :: GameName -> Update CivState (Maybe String)
deleteGame gamename = do
	updateCivLensU Nothing $ civGameLens gamename
	return Nothing

joinGame :: GameName -> PlayerName -> Update CivState (Maybe String)
joinGame gamename playername = do
	return Nothing --updateCivLensU () $ civGameLens gamename . 

$(makeAcidic ''CivState [
	'getCivState,
	'createNewGame,
	'joinGame,
	'deleteGame,
	'incTrade ])

data GameAdminAction =
	CreateGameGAA GameName |
	JoinGameGAA GameName PlayerName |
	VisitGameGAA GameName |
	StartGameGAA GameName |
	DeleteGameGAA GameName
	deriving Show

deriveJSON defaultOptions ''GameAdminAction

deriveJSON defaultOptions ''GameName
deriveJSON defaultOptions ''PlayerName

getGame gamename = queryCivLensH $ civGameLens gamename

getGamePlayer :: GameName -> PlayerName -> Handler (Maybe (Game,Maybe Player))
getGamePlayer gamename playername = do
	mb_game <- getGame gamename
	return $ case mb_game of
		Nothing -> Nothing
		Just game -> Just (game,Map.lookup playername (_gamePlayers game))

--------

initialCivState :: CivState
initialCivState = CivState $ Map.fromList [
	(GameName "testgame",Game "public@thinking-machines.net" Running (Just [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ])
		(Map.fromList [
			(PlayerName "Spieler Rot", Player Red Russia Despotism (Trade 1) (Culture 6) (Coins 1) [
				TechCard CodeOfLaws TechLevelI (Coins 2),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0),
				TechCard MonarchyTech TechLevelII (Coins 0) ]),
			(PlayerName "Spieler Blau", Player Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
				TechCard CodeOfLaws TechLevelI (Coins 1),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0),
				TechCard MonarchyTech TechLevelII (Coins 0),
				TechCard PrintingPress TechLevelII (Coins 0),
				TechCard Sailing TechLevelII (Coins 0),
				TechCard Construction TechLevelII (Coins 0),
				TechCard Engineering TechLevelII (Coins 0),
				TechCard SteamPower TechLevelIII (Coins 0),
				TechCard Banking TechLevelIII (Coins 0),
				TechCard MilitaryScience TechLevelIII (Coins 0),
				TechCard Computers TechLevelIV (Coins 0),
				TechCard MassMedia TechLevelIV (Coins 0),
				TechCard SpaceFlight TechLevelV (Coins 0) ])
			])),

	(GameName "Testgame 2", Game "public@thinking-machines.net" Waiting Nothing
		(Map.fromList [
			(PlayerName "Spieler Blau", Player Blue America Democracy (Trade 0) (Culture 0) (Coins 0) [])
			])
		)
	]

requireLoggedIn :: Handler UserName
requireLoggedIn = do
	Entity userid user <- requireAuth
	return $ userEmail user

executeGameAdminAction :: GameAdminAction -> Handler (Maybe String)
executeGameAdminAction gaa = do
	user <- requireLoggedIn
	case gaa of
		CreateGameGAA gamename -> do
			updateCivH $ CreateNewGame gamename user
		JoinGameGAA gamename playername -> do
			updateCivH $ JoinGame gamename playername
			return Nothing
		VisitGameGAA gamename -> do
			return Nothing
		StartGameGAA gamename -> do
			return Nothing
		DeleteGameGAA gamename ->
			updateCivH $ DeleteGame gamename

----
			
-- In Handler monad

updateCivH event = do
	app <- getYesod
	update' (appCivAcid app) event

queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ view lens civstate


