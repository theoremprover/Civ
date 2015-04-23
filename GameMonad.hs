module GameMonad where

import Import

import Model
import AppData
import DisplayData

createNewGame name = do
	techs1 <- mapM insert [
		TechCard CodeOfLaws TechLevelI (Coins 2),
		TechCard HorsebackRiding TechLevelI (Coins 0),
		TechCard AnimalHusbandry TechLevelI (Coins 0),
		TechCard Philosophy TechLevelI (Coins 0),
		TechCard Navigation TechLevelI (Coins 0),
		TechCard Navy TechLevelI (Coins 0),
		TechCard MonarchyTech TechLevelII (Coins 0) ]
	techs2 <- mapM insert [
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
	playerids <- mapM insert [
		Player "Spieler Rot" Red Russia Despotism (Trade 1) (Culture 6) (Coins 1) techs1,
		Player "Spieler Blau" Blue America Democracy (Trade 2) (Culture 11) (Coins 3) techs2 ]
	tileids <- mapM insert [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ]
	insert $ Game name tileids playerids

selectFromIds field ids = runDB $ selectList [ field <-. ids ] []

loadAppData gameid = do
	[game@(Entity _ (Game appDataGameName tileids playerids))] <- selectFromIds GameId [gameid]
	tiles <- selectFromIds BoardTileId tileids
	players <- selectFromIds PlayerId playerids
	return $ AppData game tiles players

loadDisplayData playerid displayscale = do
	return $ DisplayData playerid displayscale
