{-# LANGUAGE RecordWildCards #-}

module GameMonad where

import Import

import Model
import Control.Monad.Reader(local)

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

withLoadedAppData gameid contm = do
	Game gamename tileids <- runDB $ get404 gameid
	tiles <- runDB $ selectList [ BoardTileId <-. tileids ] []
	local ( \ _ -> AppData {
		appDataGameName = gamename,
		appDataTiles = tiles
		} ) contm

getAppDataSel selector = do
	app <- getYesod
	return $ selector app