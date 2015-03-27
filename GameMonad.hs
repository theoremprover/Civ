{-# LANGUAGE RecordWildCards #-}

module GameMonad where

import Import

import Model

createNewGame name = do
	tileids <- mapM insert [
		BoardTile TileSpanish 0 0 True Southward,
		BoardTile Tile1 4 0 True Eastward,
		BoardTile Tile2 0 4 True Southward,
		BoardTile Tile3 4 4 False Southward,
		BoardTile Tile4 0 8 False Southward,
		BoardTile Tile5 4 8 True Northward,
		BoardTile Tile6 0 12 True Westward,
		BoardTile TileArabs 4 12 True Northward ]
	insert $ Game name tileids

getAppDataMVar = do
	app <- getYesod
	return $ appDataMVar app

loadAppData gameid = do
	Game appDataGameName tileids <- runDB $ get404 gameid
	appDataTiles <- fmap (fmap entityVal) $ runDB $ selectList [ BoardTileId <-. tileids ] []
	appdatamvar <- getAppDataMVar
	liftIO $ putMVar appdatamvar $ AppData {..}

getAppDataSel selector = do
	appdatamvar <- handlerToWidget getAppDataMVar
	appdata <- liftIO $ readMVar appdatamvar
	return $ selector appdata