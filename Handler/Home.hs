{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Data.Aeson

import Settings.StaticFiles
import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import qualified Data.Text as Text

import Prelude(reads)
import qualified Data.Map as Map

import Data.Acid
import Data.Acid.Advanced

import Version

import GameMonad
import Model
import Entities

requireUserSessionCredentials :: Handler (UserId,User,Game,Player)
requireUserSessionCredentials = do
	requireAuthId
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing) -> redirect HomeR
		Just (userid,user,Just gameplayer) -> do
			gp <- getGamePlayer gameplayer
			case gp of
				Nothing -> invalidArgs [Text.pack $ "Player/Game " ++ show gameplayer ++ " does not exist"]
				Just (game,player) -> return (userid,user,game,player)

postHomeR :: Handler Html
postHomeR = do
	requireAuthId
	gameadminaction :: GameAdminAction <- requireJsonBody
	executeGameAdminAction gameadminaction
	displayGame

getHomeR :: Handler Html
getHomeR = do
	UserSessionCredentials usersesscred <- getUserSessionCredentials
	case usersesscred of
		Nothing -> redirect $ AuthR LoginR
		Just (_,user,_) -> do
			let email = userEmail user

			App {..} <- getYesod
			games <- query' appCivAcid GetGames

			defaultLayout $ do
				setTitle "Civ - Create, Join or Visit Game"
				
				sendJSONJulius HomeR

				[whamlet|
<h1>Games
<table border=1 cellspacing=10>
  $forall game <- games
    <tr>
      <td>#{show (gameName game)}
      <td>#{show (gameState game)}
      <td>
        $case gameState game
          $of Waiting
            <button onclick=#{onclickHandler $ JoinGame (gameName game)}>Join game
          $of Running
            <button onclick=#{onclickHandler $ VisitGame (gameName game)}>Visit
          $of Finished
      <td>
        $if gameCreator game == email
          <button onclick=#{onclickHandler $ StartGame (gameName game)}>Start Game
        $else

  <table>
    <tr>
      <td><input id="newgamename" type=text size=20>
      <td><button onclick="createGame()">Create game
|]

onclickHandler jsonobject = "sgaa(" ++ toJSONString jsonobject ++")"

toJSONString jsonobject = show $ encode jsonobject

sendJSONJulius target = toWidget [julius|

function sgaa(gameadminaction_str)
{
  xmlhttp = new XMLHttpRequest();
  xmlhttp.open("POST",'@{target}', true);
  xmlhttp.setRequestHeader("Content-type","application/json");
  xmlhttp.onreadystatechange = function() {
    if (xmlhttp.readyState == XMLHttpRequest.DONE && xmlhttp.status == 200)
    {
      document.write(xmlhttp.responseText);
      document.close();
    }
  }
  xmlhttp.send(gameadminaction_str);
}

function createGame()
{
  var gamename = document.getElementById("newgamename").value; 
  var cga = {"tag":"CreateGame","contents":gamename};
  sgaa(JSON.stringify(cga));
}

|]

postGameR :: Handler Html
postGameR = do
	displayGame

displayGame :: Handler Html
displayGame = do
	requireAuthId
	UserSessionCredentials creds <- getUserSessionCredentials
--	(userid,user,game,player) <- requireUserSessionCredentials
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		[whamlet|
<h1>#{show creds}
|]
{-
<h1>Civilization Boardgame
<p> User: #{show user}
<p> Player: #{show player}
<ul>
  $forall p <- gamePlayers game
    <li>#{show $ playerName p}: #{show $ playerTrade p}
|]
-}