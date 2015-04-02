module Handler.Home where

import Import
import GameMonad
import Model

import Handler.PlayerActionForm

playerActionForm :: Html -> MForm Handler (FormResult PlayerAction,Widget)
playerActionForm = do
	(paRes,paView) <- mreq 

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

		let tileids = map boardTileTileID $ fmap entityVal (appDataTiles appdata)

		[whamlet|
<h1>Civilization Boardgame
<form method=post action=@{HomeR}>
  #{show tileids}
  <input type=submit name=>
|]

postHomeR :: Handler Html
postHomeR = do
	((result, widget), enctype) <- runFormPost playerActionForm
	case result of
		FormSuccess playeraction -> defaultLayout $ do
			setTitle $ show playeraction