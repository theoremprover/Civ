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

{-
enumToSelect :: (Ix o,Bounded o,Show o,MonadThrow m,MonadBaseControl IO m,MonadIO m) =>
	String -> o -> WidgetT site m ()
-}
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
  xh = new XMLHttpRequest();
  xh.open("POST",'@{CommandR}', true);
  xh.timeout = 1000*60*10;
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
            <button type=button onclick="playGame(#{show $ gameName gamename})">Play Game
            as
            ^{stringListToSelect (Text.append (gameName gamename) "_playername") (Prelude.head playernametexts) playernametexts}
          $of Nothing
      <td>
        $case _gameState game
          $of Waiting
            <button type=button onclick="joinGame(#{onclickHandler $ SetSessionGameA gamename},@{WaitingR $ gameName gamename})" style="min-width: 100%">Join Game
          $of Running
            <button type=button onclick="visitGame(#{onclickHandler $ SetSessionGameA gamename},@{GameR $ gameName gamename})" style="min-width: 100%">Visit Game
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

function joinGame(action_str,url)
{
  sendAction(action_str);
  redirectTo(url);
}

function visitGame(action_str,url)
{
  sendAction(action_str);
  redirectTo(url);
}

function playGame(gamename_str)
{
  sendAction(JSON.stringify({"tag":"PlayGameA",
    "contents":[
      gamename_str,
      document.getElementById(gamename_str+"_playername").value
      ]}));
}
|]

postHomeR = pollHandler >> getHomeR

------- WaitingR

postWaitingR gn = pollHandler >> getWaitingR gn

{-
postHomeR :: Handler Html
postHomeR = do
	(userid,user) <- requireLoggedIn
	gameadminaction :: GameAdminAction <- requireJsonBody
	res <- executeGameAdminAction gameadminaction
	case res of
		Left msg -> setMessage $ toHtml msg
		Right _  -> setMessage $ toHtml (show gameadminaction)
	getHomeR

executeGameAdminAction :: GameAdminAction -> Handler UpdateResult
executeGameAdminAction gaa = do
	user <- requireLoggedIn
	printLogDebug $ "executeGameAdminAction " ++ show gaa
	case gaa of
		LongPollGAA -> do
			waitLongPoll GameAdmin
		CreateGameGAA gamename -> do
			updateCivH [GameAdmin] $ CreateNewGame gamename user
		JoinGameGAA gamename@(GameName gn) playername@(PlayerName pn) colour civ -> do
			Entity userid userentity <- requireAuth
			runDB $ Sql.update userid
				[ UserParticipations Sql.=. Map.insertWith' (++) gn [pn] (userParticipations userentity) ]
			res <- updateCivH [GameAdmin,GameGame gamename] $ JoinGame gamename playername colour civ
			case res of
				Right _ -> do
					setSession "game" gn
					setSession "player" pn
				Left _ -> do
					deleteSession "game"
					deleteSession "player"
			return res
		PlayGameGAA (GameName gn) (PlayerName pn) -> do
			setSession "game" gn
			setSession "player" pn
			return oK
		VisitGameGAA gamename@(GameName gn) -> do
			setSession "game" gn
			deleteSession "player"
			return oK
		DeleteGameGAA gamename@(GameName gn) -> do
			Entity userid userentity <- requireAuth
			runDB $ Sql.update userid
				[ UserParticipations Sql.=. Map.delete gn (userParticipations userentity) ]
			updateCivH [GameAdmin,GameGame gamename] $ DeleteGame gamename

-}

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
<button type=button onclick=#{onclickHandler $ StartGameA (GameName gn)}>Start Game
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
