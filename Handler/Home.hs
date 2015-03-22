module Handler.Home where

import Import

import Model

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
	when False $ do
		runDB $ do
			tileids <- mapM insert [
				BoardTile TileSpanish 0 0 True Southward,
				BoardTile Tile1 4 0 True Eastward,
				BoardTile Tile2 0 4 True Southward,
				BoardTile Tile3 4 4 False Southward,
				BoardTile Tile4 0 8 False Southward,
				BoardTile Tile5 4 8 True Northward,
				BoardTile Tile6 0 12 True Westward,
				BoardTile TileArabs 4 12 True Northward ]
			insert $ Game "testgame" tileids
		return ()
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		$(widgetFile "homepage")
