{-# LANGUAGE TemplateHaskell #-}

module Model2 where

import Database.Persist.TH

import Prelude

import qualified Data.Ix as Ix

type PlayerIndex = Int
type XCoor = Int
type YCoor = Int

type Trade = Int

data Orientation = Northward | Eastward | Southward | Westward
	deriving (Show,Read,Eq,Ord,Ix.Ix,Bounded)
derivePersistField "Orientation"

data Colour = Red | Green | Blue | Violet | Yellow
	deriving (Show,Read,Eq)
derivePersistField "Colour"

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Read,Eq)
derivePersistField "Civ"

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	Tile Civ
	deriving (Show,Read,Eq)
derivePersistField "TileID"
