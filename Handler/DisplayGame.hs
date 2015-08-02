module Handler.DisplayGame where

import Import hiding (map,minimum,maximum,concat,lookup)
import Prelude (map,minimum,maximum,concat,lookup)

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Array.IArray ((!),indices)
import Data.Maybe
import Control.Lens hiding (indices)

import GameMonad
import Model
import Entities
import Polls
import Handler.HandlerPolling
import Handler.StaticResources
import Lenses

colour2html :: Colour -> String
colour2html colour = show colour

-- <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	let
		toplayer playername = fromJust $ lookupAssocList playername players
		myplayerori = maybe Northward (_playerOrientation . toplayer) mb_playername

	playerareas <- mapM (playerArea game mb_playername) $ fromAssocList (_gamePlayers game)
	boardarea <- boardArea gamename mb_playername
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
		case playerareas of
			[playerarea1,playerarea2] -> [whamlet|
<div class=#{show myplayerori}>
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
	return [whamlet|
<div class=#{show (_playerOrientation player)}>
  <table>
    <tr><td>#{show playername}
    <tr><td><img src=@{dialRoute (_playerCiv player)}>
|]

boardArea :: GameName -> Maybe PlayerName -> Handler Widget
boardArea gamename mb_myplayer myplayerori = do
	Just game <- queryCivLensH $ civGameLens gamename . _Just
	Just players <- queryCivLensH $ civPlayersLens gamename
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
            <td .SquareContainer style="position:relative">
              $case arrlookup x y
                $of OutOfBounds
                $of UnrevealedSquare tileid coors
                $of sq
                  $maybe tokmarker <- _squareTokenMarker sq
                    $case tokmarker
                      $of ArtifactMarker artifact
                        <img .Center class=#{show myplayerori} src=@{artifactRoute artifact}>
                      $of HutMarker _
                        <img .Center class=#{show myplayerori} src=@{hutRoute}>
                      $of VillageMarker _
                        <img .Center class=#{show myplayerori} src=@{villageRoute}>

  <div style="z-index: 2;">
    <table .NoSpacing>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $case arrlookup x y
              $of OutOfBounds
                <td .TileContainer><img .Center src=@{transparentSquareRoute}> 
              $of UnrevealedSquare tileid coors
                $if (==) coors (Coors x y)
                  <td .TileContainer colspan=4 rowspan=4><img .Center class=#{show Northward} src=@{boardTileRoute tileid False}>
              $of sq
                $maybe (tileid,ori) <- _squareTileIDOri sq
                  <td .TileContainer colspan=4 rowspan=4><img .Center class=#{show ori} src=@{boardTileRoute tileid True}>

|]
{-
                  <img .Center alt="alt" title="#{(++) (show (x,y)) (show sq)}" src=@{transparentSquareRoute}>

	ArtifactMarker Artifact |
	HutMarker Hut |
	VillageMarker Village |
	CityMarker City |
	BuildingMarker Building
-}