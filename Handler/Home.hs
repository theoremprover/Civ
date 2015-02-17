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
	dialNavePos :: Pos,
	tradeDialSize :: Size,
	coinDialSize :: Size,
	boardSize :: Size,
	}

scalex :: Double -> Double -> Size
scalex x yfactor = (x,x*yfactor)

scaleCentric :: Size -> Proportion -> Size -> Position
scaleCentric (sourcew,sourceh) (propx,propy) (targetw,targeth) = (targetw/sourcew*propx,targeth/sourceh*propy)

defaultDisplayInfo = DisplayInfo {
	scaleCoor coor = undefined,
	squareSize    = scalex  93 1.00,
	tileSize      = scalex 372 1.00,
	vertCardSize  = scalex 122 1.54,
	horCardSize   = scalex 187 0.65,
	dialSize      = scalex 561 0.65,
	dialNaveProportion = (0.75,0.38),
	tradeDialSize = scalex 168 1.40,
	coinDialSize  = scalex  83 1.73,
	boardSize     = undefined
	}

displayInfoFactory scale game = defaultDisplayInfo {
	scaleCoor = \ coor -> show $ round $ fromIntegral coor * scale,
	boardSize = (scaleCoor $ tilesmax boardTileXCoor, scaleCoor $ tilesmax boardTileYCoor) }
	where
	tilesmax coorsel = (Prelude.maximum (map coorsel $ gameBoardTiles game) + 4) * (tileSize defaultDisplayInfo / 4)

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
		setTitle "Civilization Boardgame"
		let
			di = displayInfoFactory 0.75 game
			scaleXCoor coors = 
		$(widgetFile "homepage")
