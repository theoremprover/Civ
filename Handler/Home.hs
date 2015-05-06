{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Settings.StaticFiles
import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import Data.Text(unpack)
import Prelude(reads)

import Version

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

{-
	let myplayer = UniquePlayerName "Spieler Blau"
	(gameid,playerid :: PlayerId) <- runDB $ case userGame user of
		Nothing -> do
			let gamename = "testgame"
			mb_game <- getBy $ UniqueGameName gamename
			gid <- case mb_game of
				Nothing -> createNewGame gamename
				Just (Entity gid _) -> return gid
			Entity pid _ <- getBy404 myplayer
			update userid [ UserGame =. Just gid, UserPlayer =. Just pid ]
			return (gid,pid)
		Just gameid -> do
			Entity pid _ <- getBy404 myplayer
			return (gameid,pid)

	appdata <- loadAppData gameid
	displaydata <- loadDisplayData playerid 1.0

	player <- runDB $ get playerid
	let game = appDataGame appdata
	let players = appDataPlayers appdata
-}
	defaultLayout $ do
		setTitle "Civilization Boardgame"
{-
		let playeractionform playerid player = playerActionForm
			(ChangeTrade playerid (playerTrade player) (playerTrade player + 5)) "Add 5 Trade"
-}
		[whamlet|

<h1>Civilization Boardgame
<p> User: #{show user}
^{footer}
|]

footer = [hamlet|
<hr style="margin-bottom: 5px">
<footer>
  <small><table border=0 width="100%"><tr>
    <td align=left>Civ #{cabalVersion}
    <td align=center>GIT hash #{gitHash}
    <td align=center>compiled #{compilationDateString}
    <td align=right>Powered by Haskell Yesod <img src=@{StaticR _Yesod_gif}>
|]

