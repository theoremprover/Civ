module Handler.DisplayGame where

import Import hiding (map,minimum,maximum,concat,lookup)
import Prelude (map,minimum,maximum,concat,lookup)

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
	let
		ts = concat $ (flip map) (_gameBoardTiles game) $ \ tile ->
			let coors@(Coors x y) = _boardTileCoors tile in
				((x,y),Just tile) : [ ((x',y'),Nothing) |
					x' <- [x..(x+3)], y' <- [y..(y+3)], not (x' == x  && y' == y) ]
		xcoors = map (fst.fst) ts
		ycoors = map (snd.fst) ts
		xs = [(minimum xcoors)..(maximum xcoors)]
		ys = [(minimum ycoors)..(maximum ycoors)]
	return [whamlet|
<div .Parent>
  <div .Child style="z-index: 1;">
    <table .NoSpacing>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $maybe mb_tile <- lookup (x,y) ts
              $maybe tile <- mb_tile
                <td colspan=4 rowspan=4><img class=#{show (_boardTileOrientation tile)} src=@{boardTileRoute tile}>
              $nothing
            $nothing
              <td><img src=@{transparentSquareRoute}>

  <div .Child style="z-index: 2;">
    <table .NoSpacing border=1>
      $forall y <- ys
        <tr>
          $forall x <- xs
            <td>
              <div .Parent>
              $case mod x 2
                $of 0
                  <div .Child><img src=@{transparentSquareRoute}>
                $of 1
                  <div style="z-index=3;" .Child><img src=@{transparentSquareRoute}>
                  <div style="z-index=4;" .Child><img src=@{StaticR $ _Squares_TradeStation_jpg}>
|]
