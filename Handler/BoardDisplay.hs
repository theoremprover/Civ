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
	dialNaveProportion :: Proportion,
	tradeDialSize :: Size,
	coinDialSize :: Size,
	boardSize :: Size
	}

positionDial :: Pos -> Size -> Proportion -> Size -> Pos
positionDial (offsetx,offsety) (sourcew,sourceh) (propx,propy) (targetw,targeth) =
	(offsetx + targetw*propx - sourcew/2,
	offsety + targeth*propy - sourceh/2)

defaultDisplayInfo = DisplayInfo {
	scaleCoor     = undefined,
	squareSize    = scalex  93 1.00,
	tileSize      = scalex 372 1.00,
	vertCardSize  = scalex 122 1.54,
	horCardSize   = scalex 187 0.65,
	dialSize      = scalex 561 0.65,
	dialNaveProportion = (0.754,0.355),
	tradeDialSize = scalex 168 1.40,
	coinDialSize  = scalex  83 1.73,
	boardSize     = undefined
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

board game di = [hamlet|
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

dial game di playerindex = [hamlet|
<div .Dial .Canvas .NoSpacing>
  <img .Dial src=@{staticRoute "Dials" (playerCiv $ (gamePlayerSequence game) !! playerindex)}>
  <img .TradeDial src=@{StaticR _Images_Dials_Tradedial_gif} style=#{tradedial2style di game playerindex}>
  <img .CoinDial src=@{StaticR _Images_Dials_Coindial_gif} style=#{coindial2style di game playerindex}>
|]

tradedialDeg game playerindex =
	div (360*(playerDialTrade (gamePlayerSequence game !! playerindex) - 1)) 28

tradedial2style :: DisplayInfo -> Game -> PlayerIndex -> String
tradedial2style di game playerindex = printf "position:absolute; left:%spx; top:%spx; transform:rotate(%ideg)"
	((scaleCoor di) x)
	((scaleCoor di) y)
	(tradedialDeg game playerindex)
	where
	(x,y) = positionDial (0,0) (tradeDialSize di) (dialNaveProportion di) (dialSize di)

coindial2style :: DisplayInfo -> Game -> PlayerIndex -> String
coindial2style di game playerindex = printf "position:absolute; left:%spx; top:%spx; transform:rotate(%ideg)"
	((scaleCoor di) x)
	((scaleCoor di) y)
	(tradedialDeg game playerindex + div (360*(coins-1)) 16)
	where
	(x,y) = positionDial (0,0) (coinDialSize di) (dialNaveProportion di) (dialSize di)
	coins = playerFreeCoins (gamePlayerSequence game !! playerindex) -- TODO: Zusätzliche Coins berechnen

{-
tech2StaticR (TechCard _ tech _) = StaticR $ StaticRoute ["Images","Techs",show tech ++ ".jpg"] []

policy2StaticR policy = StaticR $ StaticRoute ["Images","Policies",show tech ++ ".jpg"] []

wonder2StaticR

	Stonehenge -> 
	Colossus -> 
	HangingGardens -> 
	TheOracle -> 
	TheGreatWall ->
	ChichenItza -> 
	Pyramids -> 
	GreatLighthouse -> 
	StatueOfZeus ->
	AngkorWat -> 
	HimejiCastle -> 
	TajMahal -> 
	PorcelainTower -> 
	MachuPichu ->
	BrandenburgGate -> 
	Louvre -> 
	NotreDame -> 
	LeonardosWorkshop ->
	SydneyOperaHouse -> 
	StatueOfLiberty -> 
	PanamaCanal -> 
	UnitedNations ->
	BigBen -> 
	CristoRedentor -> 
	Kremlin -> 
	Pentagon -> 
	TheInternet

	Infantry_1_3 -> 
	Infantry_2_2 -> 
	Infantry_3_1 ->
	Artillery_1_3 -> 
	Artillery_2_2 -> 
	Artillery_3_1 ->
	Cavalry_1_3 -> 
	Cavalry_2_2 -> 
	Cavalry_3_1 |
	Aircraft_5_7 -> 
	Aircraft_6_6 -> 
	Aircraft_7_5

	Market -> 
	Bank -> 
	Temple -> 
	Cathedral -> 
	Granary -> 
	Aquaeduct -> 
	Library -> 
	University ->
	Barracks -> 
	Academy -> 
	TradingPost -> 
	Workshop -> 
	IronMine -> 
	Harbour -> 
	Shipyard

	America -> 
	Arabs -> 
	Aztecs -> 
	China -> 
	Egypt -> 
	English -> 
	French -> 
	Germany ->
	Greeks -> 
	Indians -> 
	Japanese -> 
	Mongols -> 
	Rome -> 
	Russia -> 
	Spanish -> 
	Zulu ->
-}