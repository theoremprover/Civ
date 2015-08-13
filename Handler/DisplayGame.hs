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

data DisplayInfo = DisplayInfo {
	gameDI :: Game,
	myPlayerDI :: Maybe Player,
	myPlayerOriDI :: Orientation,
	playernameToPlayerDI :: (PlayerName -> Player)
}

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	let
		toplayer playername = fromJust $ lookupAssocList playername (_gamePlayers game)
		mb_myplayer = fmap toplayer mb_playername
		myplayerori = maybe Northward _playerOrientation mb_myplayer
		di = DisplayInfo game mb_myplayer myplayerori toplayer

	playerareas <- mapM (playerArea di) $ fromAssocList (_gamePlayers game)
	boardarea <- boardArea di
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
		case playerareas of
			[playerarea0,playerarea1] -> [whamlet|
<div class=#{show myplayerori}>
  <table>
    <tr><td>^{playerarea0}
    <tr><td>^{boardarea}
    <tr><td>^{playerarea1}
|]
			[playerarea0,playerarea1,playerarea2,playerarea3] -> [whamlet|
<table>
  <tr>
    <td rowspan="2">^{playerarea1}
    <td colspan="2">^{playerarea2}
  <tr>
    <td>^{boardarea}
    <td rowspan="2">^{playerarea3}
  <tr>
    <td colspan="2">^{playerarea0}
|]
			pas -> errHamlet $ "Layout for " ++ show (length pas) ++ " not implemented (yet)."

playerArea :: DisplayInfo -> (PlayerName,Player) -> Handler Widget
playerArea (DisplayInfo{..}) (playername,player) = do
	return [whamlet|
<div class=#{show (_playerOrientation player)}>
  <table>
    <tr><td>#{show playername}
    <tr><td><img src=@{dialRoute (_playerCiv player)}>
|]

boardArea :: DisplayInfo -> Handler Widget
boardArea (DisplayInfo{..}) = do
	let
		arr = _gameBoard gameDI
		arrlookup x y = arr!(Coors x y)
		coors = indices arr
		(xcoors,ycoors) = (map xCoor coors,map yCoor coors)
		xs = [(minimum xcoors)..(maximum xcoors)]
		ys = [(minimum ycoors)..(maximum ycoors)]
		playerori owner = _playerOrientation (playernameToPlayerDI owner)
		playercolour owner = _playerColour (playernameToPlayerDI owner)
	return [whamlet|
<div .Parent>
  <div .Child style="z-index: 1;">
    <table .NoSpacing border=1>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $with square <- arrlookup x y
              <td .SquareContainer alt="alt" title="#{(++) (show (x,y)) (show square)}" style="position:relative">
                $case square
                  $of OutOfBounds
                  $of UnrevealedSquare tileid coors
                  $of _
                    $maybe tokmarker <- _squareTokenMarker square
                      $case tokmarker
                        $of ArtifactMarker artifact
                          <img .Center class="#{show myPlayerOriDI}" src=@{artifactRoute artifact}>
                        $of HutMarker _
                          <img .Center class="#{show myPlayerOriDI}" src=@{hutRoute}>
                        $of VillageMarker _
                          <img .Center class="#{show myPlayerOriDI}" src=@{villageRoute}>
                        $of CityMarker (SecondCitySquare (Coors xc yc))
                          $maybe (CityMarker (City{..})) <- _squareTokenMarker (arrlookup xc yc)
                            <img .Center class="#{show (playerori _cityOwner)}" src=@{StaticR $ _Missing_jpg}>
                        $of CityMarker (City{..})
                          <img .Center class="#{show (playerori _cityOwner}" src=@{cityRoute (_cityType,_cityCapital,_cityWalls,playercolour _cityOwner)}>
                        $of BuildingMarker (Building buildingtype owner)
                          <img .Center class="#{show (playerori owner)}" src=@{buildingTypeRoute buildingtype}>

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
