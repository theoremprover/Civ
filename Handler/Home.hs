{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Data.Aeson

import Settings.StaticFiles
import Import

import qualified Data.Text as Text
import Text.Julius(rawJS)
import Text.Blaze(ToMarkup)

import Prelude(reads,head,tail)
import qualified Data.Map as Map
import Data.Ix

import Version

import GameMonad
import Model
import Entities
import Display

import Polls

---- Helpers ------

enumToSelect name defaultoption = do
	let rng = defaultoption : range (minBound,maxBound)
	stringListToSelect name (show defaultoption) (map show $ tail rng)

stringListToSelect :: (MonadThrow m,MonadBaseControl IO m,MonadIO m,ToMarkup o,Eq o) =>
	o -> o -> [o] -> WidgetT site m ()
stringListToSelect name defaultoption os = do
	[whamlet|
<select id=#{name} name=#{name}>
  $forall o <- os
    $case (==) o defaultoption
      $of True
        <option selected="selected">#{o}
      $of False
        <option>#{o}
|]

noPollingJulius = toWidget [julius|
function longPoll() {}
|]

longPollingJulius target affected = toWidget [julius|

xmlhttp = new XMLHttpRequest();

function longPoll()
{
  xmlhttp.open("POST",'@{target}', true);
  xmlhttp.timeout = 1000*60*10;
  xmlhttp.setRequestHeader("Content-type","application/json");
  xmlhttp.onreadystatechange = function() {
    if (xmlhttp.readyState == XMLHttpRequest.DONE)
    {
      if(xmlhttp.status == 200)
      {
        document.write(xmlhttp.responseText);
        document.close();
      }
    }
  }
  xmlhttp.ontimeout = function() { longPoll(); }
  xmlhttp.onerror = function() { longPoll(); }
  xmlhttp.send(#{rawJS $ toJSONString $ affected});
}

function redirectTo(target_str)
{
  xmlhttp.open("GET",target_str, true);
  xmlhttp.onreadystatechange = function() {
    if(xmlhttp.readyState == XMLHttpRequest.DONE)
    {
      if(xmlhttp.status == 200)
      {
        document.write(xmlhttp.responseText);
        document.close();
      }
    }
  }
  xmlhttp.ontimeout = function() { alert("Timeout loading " + target_str); }
  xmlhttp.onerror = function() { alert("Error loading " + target_str); }
  xmlhttp.send(null);
}

function sendAndRedirect(action_str,url)
{
  sendAction_Fun(action_str,
    function() { redirectTo(url); });
}

function onUnload()
{
  xmlhttp.abort();
}
|]

onclickHandler jsonobject = "sendAction(" ++ toJSONString jsonobject ++")"

toJSONString jsonobject = show $ encode jsonobject

sendJSONJulius = toWidget [julius|

function sendAction(action_str)
{
  sendAction_Fun(action_str,function() {});
}

function sendAction_Fun(action_str,fun_after_response)
{
  xh = new XMLHttpRequest();
  xh.open("POST",'@{CommandR}', true);
  xh.setRequestHeader("Content-type","application/json");
  xh.onreadystatechange = function() {
    if (xh.readyState == XMLHttpRequest.DONE)
    {
      if(xh.status == 200)
      {
        var response = JSON.parse(xh.responseText);
        if(response.hasOwnProperty("Left"))
        {
          alert(response["Left"]);
        }
        else
        {
          fun_after_response();
        }
      }
      else
      {
        alert("Status=" + xh.status + ":\n" +
          "action_str = " + action_str + "\n" +
          "Response: " + xh.responseText + "\n");
      }
    }
  }
  xh.ontimeout = function() { alert("Timeout sending " + action_str); }
  xh.onerror = function() { alert("Error sending " + action_str); }
  xh.send(action_str);
}
|]


----- HomeR ---------

getHomeR :: Handler Html
getHomeR = do
	(userid,user) <- requireLoggedIn
	let email = userEmail user

	games <- queryCivLensH $ civStateLens . civGames

	defaultLayout $ do
		setTitle "Civ - Create, Join or Visit Game"
		
		sendJSONJulius
		longPollingJulius HomeR GameAdmin

		[whamlet|
<h1>Games
<table border=1 cellspacing=10>
  $forall (gamename,game) <- Map.toList games
    <tr>
      <td>#{show gamename}
      <td>#{show (_gameState game)}
      <td>
        $case Map.lookup (gameName gamename) (userParticipations user)
          $of Just playernametexts
            $case _gameState game
              $of Waiting
                <button type=button onclick="playGame(#{show $ gameName gamename},'@{WaitingR $ gameName gamename}')">Play Game
              $of Running
                <button type=button onclick="playGame(#{show $ gameName gamename},'@{GameR $ gameName gamename}')">Play Game
              $of _
            as
            ^{stringListToSelect (Text.append (gameName gamename) "_playername") (Prelude.head playernametexts) playernametexts}
          $of Nothing
      <td>
        $case _gameState game
          $of Waiting
            <button type=button onclick="sendAndRedirect(#{toJSONString $ SetSessionGameA gamename},'@{WaitingR $ gameName gamename}')" style="min-width: 100%">Join Game
          $of Running
            <button type=button onclick="sendAndRedirect(#{toJSONString $ SetSessionGameA gamename},'@{GameR $ gameName gamename}')" style="min-width: 100%">Visit Game
          $of Finished
      <td>
        $if (&&) (_gameCreator game == email) (_gameState game /= Running)
          <button type=button onclick=#{onclickHandler $ DeleteGameA gamename} style="min-width: 100%">Delete Game
  <tr>
    <td>
      GameName
      <input id="newgamename" type=text size=20>
    <td>
    <td><button type=button onclick="createGame()" style="min-width: 100%">Create game
|]

		toWidget [julius|

function createGame()
{
  var gamename = document.getElementById("newgamename").value; 
  var cga = {"tag":"CreateGameA","contents":gamename};
  sendAction(JSON.stringify(cga));
}

function playGame(gamename_str,url)
{
  sendAndRedirect(JSON.stringify(
    { "tag":"SetSessionPlayerA",
      "contents":document.getElementById(gamename_str+"_playername").value }),
    url);
}
|]

postHomeR = pollHandler >> getHomeR

------- WaitingR

postWaitingR gn = pollHandler >> getWaitingR gn

getWaitingR :: Text -> Handler Html
getWaitingR gn = do
	(userid,user,_,game,mb_player) <- maybeVisitor
	defaultLayout $ do
		setTitle $ toHtml $ "Civ - " ++ show gn
		Just game <- getGame $ GameName gn
		sendJSONJulius
		longPollingJulius (WaitingR gn) (GameGame $ GameName gn)
		[whamlet|

<h1>Game #{gn}
<table>
  $forall (PlayerName pn,player) <- Map.toList (_gamePlayers game)
    <tr>
      <td>#{pn}
      <td fgcolor=white bgcolor=#{colour2html $ _playerColour player}>#{show $ _playerColour player}
      <td>#{show $ _playerCiv player}

  <tr>
    <td><input id="playername" type=text size=20 value="New Player">
    <td>^{enumToSelect "colour" Red}
    <td>^{enumToSelect "civ" Russia}
    <td><button type=button onclick="joinGame(#{show gn})">Join The Game
$if userEmail user == _gameCreator game
  <button type=button onclick="sendAndRedirect(#{toJSONString $ StartGameA $ GameName gn},'@{GameR gn}')">Start Game
|]

		toWidget [julius|

function joinGame(gamename_str)
{
  sendAction(JSON.stringify({"tag":"JoinGameA",
    "contents":[
      gamename_str,
      document.getElementById("playername").value,
      document.getElementById("colour").value,
      document.getElementById("civ").value ]}));
}
|]

---- GameR --------------

postGameR gn = pollHandler >> (getGameR gn)

getGameR :: Text -> Handler Html
getGameR gn = do
	(userid,user,_,game,mb_player) <- maybeVisitor
	displayGame (userid,user,GameName gn,game,mb_player)

displayGame :: (UserId,User,GameName,Game,Maybe (PlayerName,Player)) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername_player) = do
	case _gameState game of
		Waiting -> do
			setMessage $ toHtml $ "Game " ++ show gamename ++ " is waiting to start..."
			redirect $ WaitingR $ gameName gamename
		Finished -> do
			setMessage $ toHtml $ "Game " ++ show gamename ++ " is finished already."
			redirect $ HomeR
		_ -> do
			defaultLayout $ do
				setTitle "Civilization Boardgame"
				sendJSONJulius
				longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
				[whamlet|

$case mb_playername_player
  $of Nothing
    <h1>Game #{Text.concat [gameName gamename," - Visitor"]}
  $of Just (playername,_)
    <h1>Game #{Text.concat [gameName gamename," - Player ",playerName playername]}
<ul>
  $forall (pn,p) <- Map.toList $ _gamePlayers game
    <li>
      #{show pn}: #{show $ _playerTrade p}
      $case mb_playername_player
        $of Nothing
        $of Just (playername,_)
          $if playername == pn
            <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade
|]
