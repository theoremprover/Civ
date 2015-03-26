module Handler.Home where

import Import
import GameMonad
import Model

getHomeR :: Handler Html
getHomeR = do
	let gamename = "testgame"
	gameid <- runDB $ do
		mb_game <- getBy $ UniqueGameName gamename
		case mb_game of
			Nothing -> createNewGame gamename
			Just (Entity gameid game) -> return gameid

	withLoadedAppData gameid $ do

		defaultLayout $ do
			setTitle "Civilization Boardgame"
			app <- handlerToWidget getYesod
			let tiles = show $ map boardTileTileID (appDataTiles $ appData app)
			$(widgetFile "homepage")
