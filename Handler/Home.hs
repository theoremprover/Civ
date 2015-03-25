module Handler.Home where

import Import
import GameMonad
import Yesod.Handler
import Model

getHomeR :: Handler Html
getHomeR = do
	let gamename = "testgame"
	gameid <- runDB $ do
		mb_game <- getBy $ UniqueGameName gamename
		case mb_game of
			Nothing -> createNewGame gamename
			Just (Entity gameid game) -> return gameid

	loadAppData gameid

	defaultLayout $ do
		setTitle "Civilization Boardgame"
		app <- liftHandler getYesod
		let tiles = show $ appDataTiles (appData app)
		addBody $ string tiles
		$(widgetFile "homepage")
