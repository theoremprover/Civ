{-# LANGUAGE TemplateHaskell #-}

module GameMonad where

import Data.Aeson.TH

import Import

import Model
import Data.Acid
import Data.Acid.Advanced

data GameAdminAction =
	CreateGame GameName |
	JoinGame GameName
	deriving Show

deriveJSON defaultOptions ''GameAdminAction

deriveJSON defaultOptions ''GameName

getGamePlayer :: (GameName,PlayerName) -> Handler (Maybe (Game,Player))
getGamePlayer (gamename,playername) = do
	App {..} <- getYesod
	games <- query' appCivAcid GetGames
	case filter ((==gamename).gameName) games of
		[game] -> case filter ((==playername).playerName) (gamePlayers game) of
			[player] -> return $ Just (game,player)
			_ -> return Nothing
		_ -> return Nothing

--------

initialCivState :: CivState
initialCivState = CivState [
	Game (GameName "Testgame") Running [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ]
		[
			Player (PlayerName "Spieler Rot") Red Russia Despotism (Trade 1) (Culture 6) (Coins 1) [
				TechCard CodeOfLaws TechLevelI (Coins 2),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0),
				TechCard MonarchyTech TechLevelII (Coins 0) ],
			Player (PlayerName "Spieler Blau") Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
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
				TechCard SpaceFlight TechLevelV (Coins 0) ]
			],

	Game (GameName "Testgame 2") Waiting [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ]
		[
			Player (PlayerName "Spieler Blau") Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
				TechCard CodeOfLaws TechLevelI (Coins 1),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0) ]
			]

		]

{-
getGame :: User -> Handler Game
getGameAcid user = do
	App {..} <- getYesod
	 appCivAcid
	
selectFromIds field ids = runDB $ selectList [ field <-. ids ] []

loadAppData gameid = do
	[game@(Entity _ (Game appDataGameName tileids playerids))] <- selectFromIds GameId [gameid]
	tiles <- selectFromIds BoardTileId tileids
	players <- selectFromIds PlayerId playerids
	return $ AppData game tiles players

loadDisplayData playerid displayscale = do
	return $ DisplayData playerid displayscale
-}
