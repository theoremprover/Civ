{-# LANGUAGE TemplateHaskell #-}

module GameMonad where

import Data.Aeson.TH

import Import

import Model
import Data.Acid
import Data.Acid.Advanced

import Control.Lens
import qualified Data.Map as Map

data GameAdminAction =
	CreateGame GameName |
	JoinGame GameName |
	VisitGame GameName |
	StartGame GameName
	deriving Show

deriveJSON defaultOptions ''GameAdminAction

deriveJSON defaultOptions ''GameName


viewCiv lens = do
	App {..} <- getYesod
	civstate <- query' appCivAcid GetCivState
	return $ view lens civstate

getGamePlayer :: (GameName,PlayerName) -> Handler (Maybe (Game,Player))
getGamePlayer (gamename,playername) = do
	mb_game <- viewCiv $ civGames . (at gamename)

--------

queryCiv event = do
	app <- getYesod
	query' (appCivAcid app) event

updateCiv event = do
	app <- getYesod
	update' (appCivAcid app) event

--------

initialCivState :: CivState
initialCivState = CivState $ Map.fromList [
	(GameName "testgame",Game "public@thinking-machines.net" Running [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ]
		Map.fromList [
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
			]),

	(GameName "Testgame 2", Game "public@thinking-machines.net" Waiting [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ]
		Map.fromList [
			(PlayerName "Spieler Blau", Player Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
				TechCard CodeOfLaws TechLevelI (Coins 1),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0) ])
			]
		)
	]

requireLoggedIn :: Handler UserName
requireLoggedIn = do
	Entity userid user <- requireAuth
	return user

executeGameAdminAction :: GameAdminAction -> Handler (Maybe String)
executeGameAdminAction gaa = do
	user <- requireLoggedIn
	case gaa of
		CreateGame gamename -> do
			updateCiv $ CreateNewGame gamename user
		JoinGame gamename -> do
			return Nothing
		VisitGame gamename -> do
			return Nothing
		StartGame gamename -> do
			return Nothing
