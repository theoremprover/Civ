{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Data.Aeson

import Settings.StaticFiles
import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import qualified Data.Text as Text
import Text.Julius(rawJS)

import Prelude(reads)
import qualified Data.Map as Map
import Data.Ix

import Version

import GameMonad
import Model
import Entities

errRedirect :: String -> Handler a
errRedirect s = do
	setMessage $ toHtml s
	redirect HomeR

maybeVisitorUserSessionCredentials :: Handler (UserId,User,GameName,Game,Maybe (PlayerName,Player))
maybeVisitorUserSessionCredentials = do
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing,_) -> errRedirect "gamename not set in this session"
		Just (userid,user,Just gamename,mb_playername) -> do
			mb_game <- getGame gamename
			case mb_game of
				Nothing -> do
					errRedirect $ "There is no game " ++ show gamename
				Just game -> case mb_playername of
					Nothing -> return (userid,user,gamename,game,Nothing)
					Just playername -> do
						mb_gameplayer <- getGamePlayer gamename playername
						case mb_gameplayer of
							Nothing -> do
								errRedirect $ "There is no player " ++ show playername ++ " in game " ++ show gamename
							Just (game,mb_player) -> return (userid,user,gamename,game,case mb_player of
								Nothing -> Nothing
								Just player -> Just (playername,player) )

requirePlayerUserSessionCredentials :: Handler (UserId,User,GameName,Game,PlayerName,Player)
requirePlayerUserSessionCredentials = do
	(userid,user,gamename,game,mb_player) <- maybeVisitorUserSessionCredentials
	case mb_player of
		Nothing -> errRedirect "You are not a player in this game"
		Just (playername,player) -> return (userid,user,gamename,game,playername,player)

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
			setMessage $ toHtml (show gameadminaction)
			case gameadminaction of
				LongPollGAA -> do
					waitLongPoll GameAdmin
					getHomeR
				VisitGameGAA _ -> redirect GameR
				JoinGameGAA _ _ _ _ -> redirect GameR
				CreatePlayerGAA gamename -> createPlayer gamename
				_ -> getHomeR

createPlayer :: GameName -> Handler Html
createPlayer gamename@(GameName gn) = do
	defaultLayout $ do
		setTitle "Civ - Create A Player"
		sendJSONJulius HomeR
		noPolling
		[whamlet|
<h1>Create A Player
<table>
  <tr>
    <td>Player Name:
    <td><input id="playername" type=text size=20 value="New Player">
  <tr>
    <td>Colour:
    <td>^{enumToSelect "colour" Red}
  <tr>
    <td>Civilization:
    <td>^{enumToSelect "civ" Russia}
  <tr>
    <td><button type=button onclick="joinGame(#{show gn})">Join The Game
|]
		toWidget [julius|
function joinGame(gamename_str)
{
  var playername = document.getElementById("playername").value; 
  sgaa(JSON.stringify({"tag":"JoinGameGAA",
    "contents":[
      gamename_str,
      document.getElementById("playername").value,
      document.getElementById("colour").value,
      document.getElementById("civ").value ]}));
}
|]

enumToSelect :: (Ix o,Bounded o,Show o,MonadThrow m,MonadBaseControl IO m,MonadIO m) => String -> o -> WidgetT site m ()
enumToSelect name defaultoption = do
	let vals = range (minBound,maxBound)
	[whamlet|
<select id=#{name} name=#{name}>
  $forall val <- vals
    $case (==) val defaultoption
      $of True
        <option selected="selected">#{show val}
      $of False
        <option>#{show val}
|]

noPolling = toWidget [julius|
function longPoll() {}
|]

longPollingJulius action = toWidget [julius|
function longPoll()
{
  sgaa(#{rawJS $ toJSONString action});
}
|]

getHomeR :: Handler Html
getHomeR = do
	UserSessionCredentials usersesscred <- getUserSessionCredentials
	case usersesscred of
		Nothing -> redirect $ AuthR LoginR
		Just (_,user,_,_) -> do
			let email = userEmail user

			civstate <- queryCivLensH civStateLens

			mb_msg <- getMessage
			
			defaultLayout $ do
				setTitle "Civ - Create, Join or Visit Game"
				
				sendJSONJulius HomeR
				longPollingJulius LongPollGAA

				[whamlet|
$maybe msg <- mb_msg
  <div #message>#{msg}

<h1>Games
<table border=1 cellspacing=10>
  $forall (gamename,game) <- Map.toList (_civGames civstate)
    <tr>
      <td>#{show gamename}
      <td>#{show (_gameState game)}
      <td>
        $case _gameState game
          $of Waiting
            <button type=button onclick=#{onclickHandler $ CreatePlayerGAA gamename} style="min-width: 100%">Join game
          $of Running
            <button type=button onclick=#{onclickHandler $ VisitGameGAA gamename} style="min-width: 100%">Visit
          $of Finished
      <td>
        $if (&&) (_gameCreator game == email) (_gameState game == Waiting)
          <button type=button onclick=#{onclickHandler $ StartGameGAA gamename} style="min-width: 100%">Start Game
        $else
      <td>
        $if (&&) (_gameCreator game == email) (_gameState game /= Running)
          <button type=button onclick=#{onclickHandler $ DeleteGameGAA gamename} style="min-width: 100%">Delete Game
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
  var cga = {"tag":"CreateGameGAA","contents":gamename};
  sgaa(JSON.stringify(cga));
}
|]

onclickHandler jsonobject = "sgaa(" ++ toJSONString jsonobject ++")"

toJSONString jsonobject = show $ encode jsonobject

sendJSONJulius target = toWidget [julius|
function sgaa(gameadminaction_str)
{
  xmlhttp = new XMLHttpRequest();
  xmlhttp.timeout = 1000*60*10;
  xmlhttp.open("POST",'@{target}', true);
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
  xmlhttp.ontimeout = function() { sgaa(gameadminaction_str); }
  xmlhttp.onerror = function() { sgaa(gameadminaction_str); }
  xmlhttp.onabort = function() { sgaa(gameadminaction_str); }
  xmlhttp.send(gameadminaction_str);
}
|]

getGameR :: Handler Html
getGameR = do
	(userid,user,gamename,game,mb_player) <- maybeVisitorUserSessionCredentials
	case mb_player of
		Nothing -> visitGame (userid,user,gamename,game)
		Just (playername,player) -> displayGame (userid,user,gamename,game,playername,player)

postGameR :: Handler Html
postGameR = do
	(userid,user,gamename,game,playername,player) <- requirePlayerUserSessionCredentials
	gameaction :: GameAction <- requireJsonBody
	res <- executeGameAction gamename playername gameaction
	case res of
		Just msg -> do
			setMessage $ toHtml msg
		Nothing -> do
			setMessage $ toHtml $ show gameaction
	displayGame (userid,user,gamename,game,playername,player)

visitGame :: (UserId,User,GameName,Game) -> Handler Html
visitGame (userid,user,gamename,game) = do
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		[whamlet|
<h1>Civilization Boardgame
<p>Visitor not implemented yet.
|]

displayGame :: (UserId,User,GameName,Game,PlayerName,Player) -> Handler Html
displayGame (userid,user,gamename,game,playername,player) = do
	mb_msg <- getMessage
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius GameR
		longPollingJulius LongPollGA
		[whamlet|
<h1>Civilization Boardgame
$maybe msg <- mb_msg
  <div #message>#{msg}
<p>#{show playername}
<p>#{show player}
<ul>
  $forall (pn,p) <- Map.toList (_gamePlayers game)
    <li>
      #{show pn}: #{show $ _playerTrade p}
<button type=button onclick=#{onclickHandler $ IncTradeGA (Trade 1)}>IncTrade
|]