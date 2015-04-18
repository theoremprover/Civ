module Handler.Home where

import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import Data.Text(unpack)
import Prelude(reads)

import GameMonad
import Model
import AppData
import DisplayData
import ExecutePlayerAction

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

getAuthenticatedUser :: Handler (UserId,User)
getAuthenticatedUser = do
	userid <- requireAuthId
	user <- runDB $ get404 userid
	return (userid,user)

postHomeR :: Handler Html
postHomeR = do
	getAuthenticatedUser
	playeraction <- runInputPost $ ireq playerActionField "playeraction"
	executePlayerAction playeraction
	getHomeR

getHomeR :: Handler Html
getHomeR = do
	(userid,user) <- getAuthenticatedUser

	(gameid,playerid) <- case userGame user of
		Nothing -> runDB $ do
			let gamename = "testgame"
			mb_game <- getBy $ UniqueGameName gamename
			gid <- case mb_game of
				Nothing -> createNewGame gamename
				Just (Entity gid _) -> return gid
			pid <- getBy $ UniquePlayerName "Spieler Blau"
			update userid [ UserGame =. Just gid, UserPlayer =. pid ]
			return (gid,pid)
		Just gameid -> return (gameid

	appdata <- loadAppData gameid
	displaydata <- loadDisplayData playerid 1.0

	defaultLayout $ do
		setTitle "Civilization Boardgame"

		let tileids = map boardTileTileID $ fmap entityVal (appDataTiles appdata)

		let playeractionform playerid player = playerActionForm
			(ChangeTrade (fromSqlKey playerid) (playerTrade player) (playerTrade player + 5)) "Add 5 Trade"

		[whamlet|
<h1>Civilization Boardgame
<p> User: #{show user}
<p> Player: #{show player}
<p> #{show tileids}
<ul>
  $forall (Entity playerid player) <- appDataPlayers appdata
    <li>
        Player #{playerName player}: #{playerTrade player} Trade
        ^{playeractionform playerid player}
|]

