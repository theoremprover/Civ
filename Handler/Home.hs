{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Data.Aeson

import Settings.StaticFiles
import Import
import Database.Persist.Sql(fromSqlKey,toSqlKey)

import qualified Data.Text as Text

import Prelude(reads)
import qualified Data.Map as Map
import Data.Ix

import Version

import GameMonad
import Model
import Entities

maybeVisitorUserSessionCredentials :: Handler (UserId,User,GameName,Game,Maybe (PlayerName,Player))
maybeVisitorUserSessionCredentials = do
	UserSessionCredentials creds <- getUserSessionCredentials
	case creds of
		Nothing -> redirect $ AuthR LoginR
		Just (_,_,Nothing,_) -> do
			setMessage $ toHtml $ ("gamename not set in this session" :: String)
			redirect HomeR
		Just (userid,user,Just gamename,mb_playername) -> do
			mb_game <- getGame gamename
			case mb_game of
				Nothing -> do
					setMessage $ toHtml $ ("There is no game " ++ show gamename :: String)
					redirect HomeR
				Just game -> case mb_playername of
					Nothing -> return (userid,user,gamename,game,Nothing)
					Just playername -> do
						mb_gameplayer <- getGamePlayer gamename playername
						case mb_gameplayer of
							Nothing -> do
								setMessage $ toHtml $ "There is no player " ++ show playername ++ " in game " ++ show gamename
								redirect HomeR
							Just (game,mb_player) -> return (userid,user,gamename,game,case mb_player of
								Nothing -> Nothing
								Just player -> Just (playername,player) )

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
				VisitGameGAA _ -> displayGame
				JoinGameGAA _ _ _ _ -> displayGame
				CreatePlayerGAA gamename -> createPlayer gamename
				_ -> getHomeR

createPlayer :: GameName -> Handler Html
createPlayer gamename = do
	defaultLayout $ do
		setTitle "Civ - Create A Player"
		[whamlet|
<h1>Create A Player
<form action=@{HomeR} enctype='application/json' method=POST>
  <input name="gamename" type=hidden>
  <table>
    <tr>
      <td>Player Name:
      <td><input name="playername" type=text size=20>
    <tr>
      <td>Colour:
      <td>^{enumToSelect "colour" Red}
    <tr>
      <td>Civilization:
      <td>^{enumToSelect "civ" America}
  <button type="submit">Join The Game
|]

enumToSelect :: (Ix o,Bounded o,Show o,MonadThrow m,MonadBaseControl IO m,MonadIO m) => String -> o -> WidgetT site m ()
enumToSelect name defaultoption = do
	let vals = map (\ v -> (v==defaultoption,v)) $ range (minBound,maxBound)
	[whamlet|
<select name=#{name}>
  $forall (defopt,val) <- vals
    <option #{if defopt then "selected" else ""}>#{show val}
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
				createGameJulius

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
|]

createGameJulius = toWidget [julius|
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
	(userid,user,gamename,game,mb_player) <- maybeVisitorUserSessionCredentials
	mb_msg <- getMessage
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		[whamlet|
<h1>Civilization Boardgame
$maybe msg <- mb_msg
  <div #message>#{msg}
<p>User: #{show user}
$case mb_player
  $of Nothing
    <p>Visitor
  $of Just (playername,player)
    <p>#{show playername}
    <p>#{show player}
<ul>
  $forall (pn,p) <- Map.toList (_gamePlayers game)
    <li>#{show pn}: #{show $ _playerTrade p}
|]