{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Settings.StaticFiles
import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import Data.Text(unpack)
import Prelude(reads)
import qualified Data.Map as Map

import Data.Acid

import Version

import GameMonad
import Model
import Entities

{-
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
-}

data UserSessionCredentials = UserSessionCredentials {
	userSessionCredentials :: Maybe (UserId,User,Maybe (GameName,PlayerName)) }

getUserSessionCredentials :: Handler UserSessionCredentials
getUserSessionCredentials = do
	userid <- requireAuthId
	user <- runDB $ get404 userid
	let mbgameplayer = case (lookupSession "game",lookupSession "player") of
		(Just gamename,Just playername) -> Just (GameName gamename,PlayerName playername)
		_ -> Nothing
	return (userid,user,mbgameplayer)

{-
		[(gamename,[playername])] -> do
			(player,game) <- query' appCivAcid (GetPlayerGame gamename playername)
			return $ Just (player,game)
	return (mbplayergame,user)
-}

{-
postHomeR :: Handler Html
postHomeR = do
	requireAuthId
	playeraction <- runInputPost $ ireq playerActionField "playeraction"
	executePlayerAction playeraction
	getHomeR

		App {..} <- getYesod
-}

getHomeR :: Handler Html
getHomeR = do
	UserSessionCredentials {..} <- getUserSessionCredentials
	App {..} <- getYesod
	games <- query' appCivAcid GetGames

	defaultLayout $ do
		setTitle "Civ - Create, Join or Visit Game"
		
		[whamlet|
<h1>Games
  <table>
    <tr>
      <td>Name
      <td>Status
    $forall game <- games
      
|]

postGameR :: Handler Html
postGameR = do
	UserSessionCredentials {..} <- getUserSessionCredentials
	case mbplayergame of
		Nothing -> getCreateJoinGameR
		Just (player,game) -> do
			defaultLayout $ do
				setTitle "Civilization Boardgame"
		{-
				let playeractionform playerid player = playerActionForm
					(ChangeTrade playerid (playerTrade player) (playerTrade player + 5)) "Add 5 Trade"
		-}
				[whamlet|

<h1>Civilization Boardgame
<p> User: #{show user}
<p> User: #{show player}
|]


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
