{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where


import Import

import Database.Persist.Sqlite

import Handler.Board
import Handler.Board2
import Handler.BoardDisplay

import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Prelude

type Size = (Double,Double)
type Pos = (Double,Double)

data DisplayInfo = DisplayInfo {
	scaleSize :: Size -> Double -> String,
	squareSize :: Size,
	tileSize :: Size,
	vertCardSize :: Size,
	horCardSize :: Size,
	dialSize :: Size,
	dialNavePos :: Pos,
	tradeDialSize :: Size,
	coinDialSize :: Size,
	boardSize :: Size,
	squareSize :: Size,
	horCardSize :: Size,
	vertCardSize :: Size,
	dialSize :: Size,
	tradeDialSize :: Size,
	coinDialSize :: Size
	}

scalex :: Double -> Double -> Size
scalex x factor = (x,x*factor)

defaultDisplayInfo = DisplayInfo {
	scale x factor = show $ round (fromIntegral x * percent * factor),
	squareSize    = scalex  93 1.00
	tileSize      = scalex 372 1.00  -- Should be dividable by 4
	vertCardSize  = scalex 122 1.54
	horCardSize   = scalex 187 0.65
	dialSize      = scalex 561 0.65
	dialNavePos   = (423,137)
	tradeDialSize = scalex 168 1.40
	coinDialSize  = scalex  83 1.73
	boardSize     = undefined
	squareWidth   = scalex (372/4) 1.0
	

getHomeR :: Handler Html
getHomeR = do
	game <- runDB $ do
		gid <- insert $ Game [
			BoardTile TileSpanish 0 0 True Southward,
			BoardTile Tile1 4 0 True Eastward,
			BoardTile Tile2 0 4 True Southward,
			BoardTile Tile3 4 4 False Southward,
			BoardTile Tile4 0 8 False Southward,
			BoardTile Tile5 4 8 True Northward,
			BoardTile Tile6 0 12 True Westward,
			BoardTile TileArabs 4 12 True Northward ]
			[
				Player "Red" English Red 2 3 15 Democracy [Rationalism,NaturalReligion]
					[TechCard TechLevelI Writing 1, TechCard TechLevelI HorsebackRiding 0, TechCard TechLevelII DemocracyTech 2],
				Player "Green" Russia Green 3 4 16 Despotism [UrbanDevelopment,NaturalReligion,MilitaryTradition]
					[TechCard TechLevelI Metalworking 0, TechCard TechLevelI HorsebackRiding 0 ]
				]
			StartOfTurn 0

		insert $ Games "testgame" gid
		get404 gid
	defaultLayout $ do
		setTitle "Civ"
		let
			percent       = 0.75 :: Double
			scale :: Int -> Double -> String
			scale x factor = show $ round (fromIntegral x * percent * factor)

			squareSize    =  93
			tileSize      = 372  -- Should be dividable by 4
			vertCardXSize = 122
			horCardXSize  = 187
			dialSize      = 561
			(dialNaveX,dialNaveY) = (423,137)
			tradeDialSize = 168
			coinDialSize  =  83
			
			boardtilesMax coorsel = (Prelude.maximum (map coorsel $ gameBoardTiles game) + 4) * (div tileSize 4)
			boardWidth  = scale (boardtilesMax boardTileXcoor) 1.0
			boardHeight = scale (boardtilesMax boardTileYcoor) 1.0
			tileWidth   = scale tileSize 1.0
			squareWidth = scale squareSize 1.0
			horCardWidth = scale horCardXSize 1.0
			horCardHeight = scale horCardXSize 0.65
			vertCardWidth = scale vertCardXSize 1.0
			vertCardHeight = scale vertCardXSize 1.54
			dialWidth = scale dialSize 1.0
			dialHeight = scale dialSize 0.65
			tradeDialWidth = scale tradeDialSize 1.0
			tradeDialHeight = scale tradeDialSize 1.4
			coinDialWidth  = scale coinDialSize 1.0
			coinDialHeight = scale coinDialSize 1.73
			
			
		$(widgetFile "homepage")
