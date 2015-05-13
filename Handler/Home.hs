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

{-
		[(gamename,[playername])] -> do
			(player,game) <- query' appCivAcid (GetPlayerGame gamename playername)
			return $ Just (player,game)
	return (mbplayergame,user)

postHomeR :: Handler Html
postHomeR = do
	requireAuthId
	playeraction <- runInputPost $ ireq playerActionField "playeraction"
	executePlayerAction playeraction
	getHomeR
-}

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
	gameadminaction <- requireJsonBody
	case gameadminaction of
		CreateGame -> 
		JoinGame ->

getHomeR :: Handler Html
getHomeR = do
	requireAuthId

	App {..} <- getYesod
	games <- query' appCivAcid GetGames

	defaultLayout $ do
		setTitle "Civ - Create, Join or Visit Game"
		
		[whamlet|
<h1>Games
  <table>
    <tr>
      <td>Name
      <td>State
    $forall game <- games
      <tr>
        <td>#{show (gameName game)}
        <td>#{show (gameState game)}
        <td>
          $case gameState game
            $of Waiting numplayers
              $if (>) numplayers (length $ gamePlayers game)
                Join game
              $else
                Full
            $of Running
              Visit
            $of Finished
|]

postGameR :: Handler Html
postGameR = do
	(userid,user,game,player) <- requireUserSessionCredentials
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		[whamlet|

<h1>Civilization Boardgame
<p> User: #{show user}
<p> User: #{show player}
|]
