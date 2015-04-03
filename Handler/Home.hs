module Handler.Home where

import Import

import Data.Text(unpack)

import Prelude(reads)

import GameMonad
import Model

playerActionField :: Field Handler PlayerAction
playerActionField = Field {
	fieldParse = \ raws _fileVals -> return $ case Prelude.reads (concatMap Data.Text.unpack raws) of
		[(playeraction,_)] -> Right $ Just playeraction
		_ -> Left $ fromString ("Could not parse as PlayerAction: " ++ show raws),
	fieldView = undefined,
	fieldEnctype = UrlEncoded
	}

playerActionForm :: PlayerAction -> String -> Widget
playerActionForm playeraction buttonname = [whamlet|
<form action=@{HomeR} method=post>
  <input type=hidden name=playeraction value="#{show playeraction}">
  <button>#{buttonname}
|]

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

		let playeractionform playerid player = playerActionForm (ChangeTrade playerid (playerTrade player) (playerTrade player + 5)) "Add 5 Trade"
		[whamlet|
<h1>Civilization Boardgame
<p> #{show tileids}
<ul>
  $forall (Entity playerid player) <- appDataPlayers appdata
    <li>
        Player #{playerName player}: #{playerTrade player} Trade
        ^{playeractionform playerid player}
|]

postHomeR :: Handler Html
postHomeR = do
	playeraction <- runInputPost $ ireq playerActionField "playeraction"
	defaultLayout $ do
		setTitle "postHomeR"
		[whamlet| <p>#{show playeraction} |]
