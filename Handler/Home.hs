{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Data.Aeson

import Settings.StaticFiles
import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import qualified Data.Text as Text

import Prelude(reads)
import qualified Data.Map as Map

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
	res <- executeGameAdminAction gameadminaction
	case res of
		Nothing -> return ()
		Just msg -> setMessage $ toHtml msg
	getHomeR

getHomeR :: Handler Html
getHomeR = do
	UserSessionCredentials usersesscred <- getUserSessionCredentials
	case usersesscred of
		Nothing -> redirect $ AuthR LoginR
		Just (_,user,_) -> do
			let email = userEmail user

			civstate <- queryCivLensH civStateLens

			mb_msg <- getMessage
			
			defaultLayout $ do
				setTitle "Civ - Create, Join or Visit Game"
				
				sendJSONJulius HomeR

				[whamlet|
$maybe msg <- mb_msg
  <div #message>#{msg}

<h1>Games
<table border=1 cellspacing=10>
  $forall (gamename,game) <- Map.toList (_civGames civstate)
    <tr>
      <td>#{show gamename}
      <td>#{show (gameState game)}
      <td>
        $case gameState game
          $of Waiting
            <button onclick=#{onclickHandler $ JoinGameGAA gamename}>Join game
          $of Running
            <button onclick=#{onclickHandler $ VisitGameGAA gamename}>Visit
          $of Finished
      <td>
        $if (&&) (gameCreator game == email) (gameState game == Waiting)
          <button onclick=#{onclickHandler $ StartGameGAA gamename}>Start Game
        $else
      <td>
        $if (&&) (gameCreator game == email) (gameState game /= Running)
          <button onclick=#{onclickHandler $ DeleteGameGAA gamename}>Delete Game
  <tr>
    <td>
      GameName
      <input id="newgamename" type=text size=20>
    <td>
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
  var cga = {"tag":"CreateGameGAA","contents":gamename};
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