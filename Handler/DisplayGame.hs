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
import Handler.StaticResources

colour2html :: Colour -> String
colour2html colour = show colour

-- <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	playerareas <- mapM (playerArea game mb_playername) $ _gamePlayers game
	boardarea <- boardArea game
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
		case playerareas of
			[playerarea1,playerarea2] -> [whamlet|
<table>
  <tr><td>^{playerarea2}
  <tr><td>^{boardarea}
  <tr><td>^{playerarea1}
|]
			[playerarea1,playerarea2,playerarea3,playerarea4] -> [whamlet|
<table>
  <tr>
    <td rowspan="2">^{playerarea2}
    <td colspan="2">^{playerarea3}
  <tr>
    <td>^{boardarea}
    <td rowspan="2">^{playerarea4}
  <tr>
    <td colspan="2">^{playerarea1}
|]
			pas -> errHamlet $ "Layout for " ++ show (length pas) ++ " not implemented (yet)."

playerArea :: Game -> Maybe PlayerName -> (PlayerName,Player) -> Handler Widget
playerArea game mb_playername (playername,player) = do
	let revealed = mb_playername == Just playername
	return [whamlet|
<div>
  <table>
    <tr><td>#{show playername}
    <tr><td><img src=@{dialRoute True (_playerCiv player)}>
|]

boardArea :: Game -> Handler Widget
boardArea game = do
	return [whamlet|
<div>
  #{show $ _gameBoardTiles game}
|]
