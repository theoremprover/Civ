{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BoardDisplay where

import Import
import qualified Prelude

import Text.Hamlet (HtmlUrl, hamlet)
--import Data.Text (Text)
--import Text.Blaze.Html.Renderer.String (renderHtml)

import Handler.Board
import Handler.Board2

import Text.Printf
--import Text.Regex.Base
import Text.Regex.TDFA
import Data.Typeable
import Data.List((!!))

type Coor = Double
type Size = (Coor,Coor)
type Pos = (Coor,Coor)
type Proportion = (Double,Double)

data DisplayInfo = DisplayInfo {
	scaleCoor :: Coor -> String,
	squareSize :: Size,
	tileSize :: Size,
	vertCardSize :: Size,
	horCardSize :: Size,
	dialSize :: Size,
	dialNaveProp :: Proportion,
	tradeDialSize :: Size,
	coinDialSize :: Size,
	boardSize :: Size,
	techCardCoinProp :: Proportion,
	coinSize :: Size,
	coinDistanceProp :: Proportion,
	dialCoinProp :: Proportion,
	governmentProp :: Proportion
	}

defaultDisplayInfo = DisplayInfo {
	scaleCoor     = undefined,
	squareSize    = scalex  93 1.00,
	tileSize      = scalex 372 1.00,
	vertCardSize  = scalex 122 1.54,
	horCardSize   = scalex 187 0.65,
	dialSize      = scalex 561 0.65,
	dialNaveProp  = (0.754,0.355),
	tradeDialSize = scalex 168 1.40,
	coinDialSize  = scalex  83 1.73,
	boardSize     = undefined,
	techCardCoinProp = (0.20,0.40),
	coinSize      = scalex  32 1.0,
	coinDistanceProp = (1.1,0),
	dialCoinProp = (0.25,0.05),
	governmentProp = (0.02,0.43)
	}
	where
	scalex :: Double -> Double -> Size
	scalex x yfactor = (x,x*yfactor)


displayInfoFactory :: Double -> Game -> DisplayInfo
displayInfoFactory scale game = defaultDisplayInfo {
	scaleCoor = \ coor -> show $ round $ coor * scale,
	boardSize = (tilesmaxx,tilesmaxy) }
	where
	tilesmaxx = (fromIntegral $ Prelude.maximum (map boardTileXcoor $ gameBoardTiles game) + 4) * (fst (tileSize defaultDisplayInfo) / 4)
	tilesmaxy = (fromIntegral $ Prelude.maximum (map boardTileYcoor $ gameBoardTiles game) + 4) * (snd (tileSize defaultDisplayInfo) / 4)

tile2StaticR :: BoardTile -> Route App
tile2StaticR (BoardTile tileid _ _ discovered _) = StaticR $
	case (((=~)::String -> String -> Bool) (show tileid) "Tile[0-9]+",discovered) of
		(False,False) -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_back.jpg" ] []
		(False,True)  -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_front.jpg" ] []
		(True,False)  -> StaticRoute [ "Images","Tiles","Back.jpg" ] []
		(True,True)   -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ ".jpg" ] []

staticRoute :: (Show a) => String -> a -> Route App
staticRoute folder a = StaticR $ StaticRoute ["Images",toPathPiece folder,toPathPiece (show a) ++ ".jpg"] []

scaleXCoor di sizef = (scaleCoor di) $ fst (sizef di)
scaleYCoor di sizef = (scaleCoor di) $ snd (sizef di)

board di game = [hamlet|
<div .Board .Canvas .NoSpacing>
  $forall boardtile <- gameBoardTiles game
    <img src=@{tile2StaticR boardtile} .#{tile2class boardtile} style=#{tile2style boardtile}>
|]
	where
	tile2class :: BoardTile -> String
	tile2class boardtile = "Tile " ++ show (boardTileOrientation boardtile)

	tile2style :: BoardTile -> String
	tile2style boardtile = printf "position:absolute; left:%spx; top:%spx;"
		((scaleCoor di) (fromIntegral (boardTileXcoor boardtile) * (fst (tileSize di) / 4)))
		((scaleCoor di) (fromIntegral (boardTileYcoor boardtile) * (snd (tileSize di) / 4)))

dial di game playerindex = [hamlet|
<div .Dial .Canvas .NoSpacing>
  <img .Dial src=@{staticRoute "Dials" (playerCiv player)}>
  <img .TradeDial src=@{StaticR $ StaticRoute [ "Images","Dials","Tradedial.gif" ] []} style=#{tradedial2style di game playerindex}>
  <img .CoinDial src=@{StaticR $ StaticRoute [ "Images","Dials","Coindial.gif" ] []} style=#{coindial2style di game playerindex}>
  ^{coins di (dialSize di) (dialCoinProp di) (playerFreeCoins player)}
  <img .VertCard src=@{staticRoute "Government" (playerGovernment player)} style=#{positionProp2style di (dialSize di) (governmentProp di)}>
|]
	where
	player :: Player
	player = (gamePlayerSequence game) !! playerindex

tradedialDeg game playerindex =
	div (360*(playerDialTrade (gamePlayerSequence game !! playerindex) - 1)) 28

positionProp2style :: DisplayInfo -> Size -> Proportion -> String
positionProp2style di (w,h) (px,py) = printf "position:absolute; left:%spx; top:%spx; "
	((scaleCoor di) (w*px))
	((scaleCoor di) (h*py))

positionDial :: Pos -> Size -> Proportion -> Size -> Pos
positionDial (offsetx,offsety) (sourcew,sourceh) (propx,propy) (targetw,targeth) =
	(offsetx + targetw*propx - sourcew/2,
	offsety + targeth*propy - sourceh/2)

tradedial2style :: DisplayInfo -> Game -> PlayerIndex -> String
tradedial2style di game playerindex = printf "position:absolute; left:%spx; top:%spx; transform:rotate(%ideg)"
	((scaleCoor di) x)
	((scaleCoor di) y)
	(tradedialDeg game playerindex)
	where
	(x,y) = positionDial (0,0) (tradeDialSize di) (dialNaveProp di) (dialSize di)

coindial2style :: DisplayInfo -> Game -> PlayerIndex -> String
coindial2style di game playerindex = printf "position:absolute; left:%spx; top:%spx; transform:rotate(%ideg)"
	((scaleCoor di) x)
	((scaleCoor di) y)
	(tradedialDeg game playerindex + div (360*(coins-1)) 16)
	where
	(x,y) = positionDial (0,0) (coinDialSize di) (dialNaveProp di) (dialSize di)
	coins = playerFreeCoins (gamePlayerSequence game !! playerindex) -- TODO: Zusätzliche Coins berechnen

coins di (targetw,targeth) (propx,propy) num = [hamlet|
  $forall i <- is
    <img .Coin style=#{coinstyle i} src=@{StaticR $ StaticRoute ["Images","Dials","Coin.gif"] []}>
|]
	where
	is = [0..(num-1)] :: [Int]
	coinstyle :: Int -> String
	coinstyle i = printf "position:absolute; left:%spx; top:%spx;"
		(scaleCoor di $ targetw*propx + fst (coinDistanceProp di) * fst (coinSize di) * (fromIntegral i))
		(scaleCoor di $ targeth*propy + snd (coinDistanceProp di) * snd (coinSize di) * (fromIntegral i))

techCard di techcard = [hamlet|
  <div .Canvas .NoSpacing>
    <img .HorCard src=@{staticRoute "Tech" (techCardTech techcard)}>
    ^{coins di (horCardSize di) (techCardCoinProp di) (techCardCoins techcard)}
|]

techTree di game playerindex = [hamlet|
  <table border=0>
    $forall (i,techcards) <- techss
      <tr>
        $forall j <- Prelude.replicate i 0
          <td>
            <br>
        $forall techcard <- techcards
          <td colspan=2>
            ^{techCard di techcard}
|]
	where
	techs :: [TechCard]
	techs = playerTechTree ((gamePlayerSequence game) !! playerindex)
	techss :: [(Int,[TechCard])]
	techss = Prelude.map (\ (i,level) -> (i,filter ((==level).techCardTreeLevel) techs))
		[(4,TechLevelV),(3,TechLevelIV),(2,TechLevelIII),(1,TechLevelII),(0,TechLevelI)]

playerArea di game playerindex rotation = [hamlet|
<div .NoSpacing style="transform: rotate(#{show rotation}deg)">
  <table .NoSpacing>
    <tr>
      <td>
        ^{techTree di game playerindex}
      <td>
        ^{dial di game playerindex}
      <td>
|]
