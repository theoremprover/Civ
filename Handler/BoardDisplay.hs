{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BoardDisplay where

import Import

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

tile2StaticR :: BoardTile -> Route App
tile2StaticR (BoardTile tileid _ _ discovered _) = StaticR $
	case (((=~)::String -> String -> Bool) (show tileid) "Tile[0-9]+",discovered) of
		(False,False) -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_back.jpg" ] []
		(False,True)  -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_front.jpg" ] []
		(True,False)  -> StaticRoute [ "Images","Tiles","Back.jpg" ] []
		(True,True)   -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ ".jpg" ] []

tile2class :: BoardTile -> String
tile2class boardtile = "Tile " ++ show (boardTileOrientation boardtile)

tile2style :: (Int -> Double -> String) -> Int -> XCoor -> YCoor -> String
tile2style displayinfo x y =
	printf "position:absolute; left:%spx; top:%spx;" (scale (x * div tilesize 4) 1.0) (scale (y * div tilesize 4) 1.0)

staticRoute :: (Show a) => String -> a -> Route App
staticRoute folder a = StaticR $ StaticRoute ["Images",toPathPiece folder,toPathPiece (show a) ++ ".jpg"] []

board game displayinfo tilesize = [hamlet|
<div .Board .Canvas .NoSpacing>
  $forall boardtile <- gameBoardTiles game
    <img src=@{tile2StaticR boardtile} .#{tile2class boardtile} style=#{tile2style scale tilesize (boardTileXcoor boardtile) (boardTileYcoor boardtile)}>
|]

dial game displayinfo playerindex = [hamlet|
<div .Dial .Canvas .NoSpacing>
  <img .Dial src=@{staticRoute "Dials" (playerCiv $ (gamePlayerSequence game) !! playerindex)}>
  <img .TradeDial src=@{StaticR _Images_Dials_Tradedial_jpg} style=#{tradedial2style displayinfo}>
  <img .CoinDial src=@{StaticR _Images_Dials_Coindial_jpg} style=#{coindial2style displayinfo}>
|]

tradedial2style scale = 

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