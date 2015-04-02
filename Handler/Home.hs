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

	loadAppData gameid
	setDisplayData 0 1.0

	defaultLayout $ do
		setTitle "Civilization Boardgame"

		appdata <- getAppData
		displaydata <- getDisplayData

		let tileids = map boardTileTileID (appDataTiles appdata)

		[whamlet|
<h1>Civilization Boardgame
#{show tileids}
|]

postPostR :: Handler Html
postPostR = do
	