module GameMonad where

import Import

import Model
import AppData
import DisplayData

createNewGame name = do
	playerids <- mapM insert [
		Player "Spieler Rot" Red Russia 1,
		Player "Spieler Blau" Blue America 2 ]
	tileids <- mapM insert [
		BoardTile (Tile Russia) 0 0 True Southward,
		BoardTile Tile1 4 0 True Eastward,
		BoardTile Tile2 0 4 True Southward,
		BoardTile Tile3 4 4 False Southward,
		BoardTile Tile4 0 8 False Southward,
		BoardTile Tile5 4 8 True Northward,
		BoardTile Tile6 0 12 True Westward,
		BoardTile (Tile America) 4 12 True Northward ]
	insert $ Game name tileids playerids

selectFromIds field ids = runDB $ selectList [ field <-. ids ] []

loadAppData gameid = do
	[game@(Entity _ (Game appDataGameName tileids playerids))] <- selectFromIds GameId [gameid]
	tiles <- selectFromIds BoardTileId tileids
	players <- selectFromIds PlayerId playerids
	return $ AppData game tiles players

loadDisplayData playerid displayscale = do
	return $ DisplayData playerid displayscale
