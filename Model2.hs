{-# LANGUAGE TemplateHaskell #-}

module Model2 where

import Database.Persist.TH

import Prelude

import qualified Data.Ix as Ix

newtype PlayerIndex = PlayerIndex Int deriving (Show,Read)
derivePersistField "PlayerIndex"
data Coors = Coors Int Int deriving (Show,Read)
derivePersistField "Coors"

newtype Trade = Trade Int deriving (Show,Read,Num)
derivePersistField "Trade"
newtype Coins = Coins Int deriving (Show,Read,Num)
derivePersistField "Coins"
newtype Culture = Culture Int deriving (Show,Read,Num)
derivePersistField "Culture"

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

data Phase = StartOfTurn | Trading | CityManagement | Movement | Research
	deriving (Show,Read,Eq,Ord,Enum)
derivePersistField "Phase"

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	CivilService | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamPower | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplaceableParts | Flight | Plastics | Combustion | AtomicTheory |
	SpaceFlight
	deriving (Show,Read)
derivePersistField "Tech"

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Read,Eq,Ord,Enum)
derivePersistField "TechLevel"

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Read)
derivePersistField "Government"

