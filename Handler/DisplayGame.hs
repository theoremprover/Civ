module Handler.DisplayGame where

import Import
import qualified Prelude

import qualified Data.Text as Text
import qualified Data.Map as Map

import GameMonad
import Model
import Entities
import Polls
import Handler.HandlerPolling

colour2html :: Colour -> String
colour2html colour = show colour

-- <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
		nPlayersLayout $ _gamePlayers game

	where
	nPlayersLayout [player1,player2] = [whamlet|
<table>
  <tr><td>^{playerArea player2}
  <tr><td>^{boardArea game}
  <tr><td>^{playerArea player1}
|]
	nPlayersLayout [player1,player2,player3,player4] = [whamlet|
<table>
  <tr>
    <td rowspan="2">^{playerArea player2}
    <td colspan="2">^{playerArea player3}
  <tr>
    <td>^{boardArea game}
    <td rowspan="2">^{playerArea player4}
  <tr>
    <td colspan="2">^{playerArea player1}
|]
	nPlayersLayout players = errHandler $ "Layout for " ++ show players ++ " not implemented (yet)."

	playerArea (playername,player) = do
		let isMyPlayer = Just playername == mb_playername
		[whamlet|

|]

	boardArea game = [whamlet|
|]
