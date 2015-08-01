module Handler.DisplayGame where

import Import hiding (map,minimum,maximum,concat,lookup)
import Prelude (map,minimum,maximum,concat,lookup)

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Array.IArray ((!),indices)

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
	playerareas <- mapM (playerArea game mb_playername) $ fromAssocList (_gamePlayers game)
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
    <tr><td><img src=@{dialRoute (_playerCiv player)}>
|]

boardArea :: Game -> Handler Widget
boardArea game = do
	let
		arr = _gameBoard game
		arrlookup x y = arr!(Coors x y)
		coors = indices arr
		(xcoors,ycoors) = (map xCoor coors,map yCoor coors)
		xs = [(minimum xcoors)..(maximum xcoors)]
		ys = [(minimum ycoors)..(maximum ycoors)]

	return [whamlet|
<div .Parent>
  <div .Child style="z-index: 1;">
    <table .NoSpacing border=1>
      $forall y <- ys
        <tr>
          $forall x <- xs
            <td style="position:relative">
              $with sq <- arrlookup x y
                <img alt="alt" title="#{(++) (show (x,y)) (show sq)}" src=@{transparentSquareRoute}>

  <div style="z-index: 2;">
    <table .NoSpacing>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $case arrlookup x y
              $of OutOfBounds
                <td><img src=@{transparentSquareRoute}> 
              $of UnrevealedSquare tileid coors
                $if (==) coors (Coors x y)
                  <td colspan=4 rowspan=4><img class=#{show Northward} src=@{boardTileRoute tileid False}>
              $of sq
                $maybe (tileid,ori) <- _squareTileIDOri sq
                  <td colspan=4 rowspan=4><img class=#{show ori} src=@{boardTileRoute tileid True}>

|]
{-
                $case arrlookup x y
                $of False
                  <img style="position: absolute; top:3px; left:3px" src=@{StaticR $ _Squares_TradeStation_jpg}>
                $of True
-}