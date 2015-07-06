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

displayGame :: (UserId,User,GameName,Game,Maybe (PlayerName,Player)) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername_player) = do
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
		
		case _gamePlayers game of
			[player1,player2] -> twoPlayer players
			_ -> errHandler $ "Layout for " ++ show players ++ " not implemented (yet)."

		where
		twoPlayer 
		[whamlet|
<table>
  <tr>
  <tr>

|]
-- <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade
