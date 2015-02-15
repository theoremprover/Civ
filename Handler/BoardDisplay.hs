{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BoardDisplay where

import Import

import Text.Hamlet (HtmlUrl, hamlet)
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Handler.Board
import Handler.Board2

import Text.Printf
--import Text.Regex.Base
import Text.Regex.TDFA
import Data.Typeable

tile2StaticR :: BoardTile -> Route App
tile2StaticR (BoardTile tileid _ _ discovered _) = StaticR $
	case (((=~)::String -> String -> Bool) (show tileid) "Tile[0-9]+",discovered) of
		(False,False) -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_back.jpg" ] []
		(False,True)  -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_front.jpg" ] []
		(True,False)  -> StaticRoute [ "Images","Tiles","Back.jpg" ] []
		(True,True)   -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ ".jpg" ] []

tile2class :: BoardTile -> String
tile2class boardtile = "Tile " ++ show (boardTileOrientation boardtile)

tile2style :: Int -> XCoor -> YCoor -> String
tile2style tilesize x y =
	printf "position:absolute; left:%ipx; top:%ipx;" (div (x*tilesize) 4) (div (y*tilesize) 4)

class (Show a,Typeable a) => ToStaticR a where
	staticRoute :: a -> Route App
	staticRoute a =
		StaticR $ StaticRoute ["Images",toPathPiece (show $ typeOf a),toPathPiece (show a) ++ ".jpg"] []

instance ToStaticR Tech 

techTree playerindex = renderHtml [hamlet|
<p>TEST!
|]

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