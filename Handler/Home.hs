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

requireUserSessionCredentials :: Handler (UserId,User,GameName,Game,PlayerName,Player)
requireUserSessionCredentials = do
	requireAuthId
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing) -> redirect HomeR
		Just (userid,user,Just gameplayer@(gamename,playername)) -> do
			gp <- getGamePlayer gameplayer
			case gp of
				Nothing -> invalidArgs [Text.pack $ "Player/Game " ++ show gameplayer ++ " does not exist"]
				Just (game,player) -> return (userid,user,gamename,game,playername,player)

postHomeR :: Handler Html
postHomeR = do
	requireAuthId
	gameadminaction :: GameAdminAction <- requireJsonBody
	res <- executeGameAdminAction gameadminaction
	case res of
		Just msg -> do
			setMessage $ toHtml msg
			getHomeR
		Nothing -> do
--			setMessage $ toHtml (show gameadminaction)
			case gameadminaction of
				VisitGameGAA _ -> displayGame
				JoinGameGAA _ _ -> displayGame
				_ -> getHomeR

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
      $case gameState game
        $of Waiting
          <td><input id="newgamename" type=text size=20>
          <td><button onclick="joinGame(#{show gamename})" width=100%>Join game
        $of Running
          <td>
          <td><button onclick=#{onclickHandler $ VisitGameGAA gamename}>Visit
        $of Finished
          <td>
          <td>
      <td>
        $if (&&) (gameCreator game == email) (gameState game == Waiting)
          <button onclick=#{onclickHandler $ StartGameGAA gamename} width=100%>Start Game
        $else
      <td>
        $if (&&) (gameCreator game == email) (gameState game /= Running)
          <button onclick=#{onclickHandler $ DeleteGameGAA gamename} width=100%>Delete Game
  <tr>
    <td>
      GameName
      <input id="newgamename" type=text size=20>
    <td>
    <td>
    <td><button onclick="createGame()" width=100%>Create game
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

function joinGame(gamename_str)
{
  var playername = document.getElementById("joinplayername").value; 
  var cga = {"tag":"JoinGameGAA","contents":gamename_str,"contents":playername};
  sgaa(JSON.stringify(cga));
}
|]

postGameR :: Handler Html
postGameR = do
	displayGame

displayGame :: Handler Html
displayGame = do
	requireAuthId
	(userid,user,gamename,game,playername,player) <- requireUserSessionCredentials
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		[whamlet|
<h1>Civilization Boardgame
<p> User: #{show user}
<p> Player: #{show playername}
<ul>
  $forall (pn,p) <- Map.toList (gamePlayers game)
    <li>#{show pn}: #{show $ playerTrade p}
|]
