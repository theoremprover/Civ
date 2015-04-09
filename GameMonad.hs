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
		BoardTile TileRussia 0 0 True Southward,
		BoardTile Tile1 4 0 True Eastward,
		BoardTile Tile2 0 4 True Southward,
		BoardTile Tile3 4 4 False Southward,
		BoardTile Tile4 0 8 False Southward,
		BoardTile Tile5 4 8 True Northward,
		BoardTile Tile6 0 12 True Westward,
		BoardTile TileAmerica 4 12 True Northward ]
	insert $ Game name tileids playerids

{-
getAppDataMVar = getYesod >>= return . appDataMVar
getAppData = handlerToWidget getAppDataMVar >>= liftIO . readMVar

getDisplayDataMVar = getYesod >>= return . appDisplayDataMVar
getDisplayData = handlerToWidget getDisplayDataMVar >>= liftIO . readMVar
-}

selectFromIds field ids = runDB $ selectList [ field <-. ids ] []
updateFromEntities entities = runDB $ forM entities $ \ (Entity eid val) -> replace eid val

loadAppData gameid = do
	[game@(Entity _ (Game appDataGameName tileids playerids))] <- selectFromIds GameId [gameid]
	tiles <- selectFromIds BoardTileId tileids
	players <- selectFromIds PlayerId playerids
	return $ AppData game tiles players

loadDisplayData whoami displayscale = do
	return $ DisplayData whoami displayscale
