{-# LANGUAGE
	CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Model where

import Prelude
import Data.Data
import Data.Ix
import Data.Text (Text(..))
import Data.Typeable
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.List
import Control.Lens
import qualified Data.Map as Map

import Entities

import qualified Data.Ix as Ix

data Coors = Coors Int Int deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy 0 'base ''Coors)

newtype Trade = Trade Int deriving (Show,Read,Num,Data,Typeable)
$(deriveSafeCopy 0 'base ''Trade)
newtype Coins = Coins Int deriving (Show,Read,Num,Data,Typeable)
$(deriveSafeCopy 0 'base ''Coins)
newtype Culture = Culture Int deriving (Show,Read,Num,Data,Typeable)
$(deriveSafeCopy 0 'base ''Culture)

data Orientation = Northward | Eastward | Southward | Westward
	deriving (Show,Read,Eq,Ord,Ix.Ix,Bounded,Data,Typeable)
$(deriveSafeCopy 0 'base ''Orientation)

data Colour = Red | Green | Blue | Violet | Yellow
	deriving (Show,Read,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy 0 'base ''Colour)

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Read,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy 0 'base ''Civ)

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	Tile Civ
	deriving (Show,Read,Eq,Data,Typeable)
$(deriveSafeCopy 0 'base ''TileID)

data Phase = StartOfTurn | Trading | CityManagement | Movement | Research
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable)
$(deriveSafeCopy 0 'base ''Phase)

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	CivilService | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamPower | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplaceableParts | Flight | Plastics | Combustion | AtomicTheory |
	SpaceFlight
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy 0 'base ''Tech)

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable)
$(deriveSafeCopy 0 'base ''TechLevel)

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy 0 'base ''Government)

data BoardTile = BoardTile {
	_boardTileId :: TileID,
	_boardTileCoors :: Coors,
	_boardTileDiscovered :: Bool,
	_boardTileOrientation :: Orientation
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy 0 'base ''BoardTile)
makeLenses ''BoardTile

data TechCard = TechCard {
	_techCardTechId :: Tech,
	_techCardLevel :: TechLevel,
	_techCardCoins :: Coins
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy 0 'base ''TechCard)
makeLenses ''TechCard

newtype PlayerName = PlayerName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy 0 'base ''PlayerName)

data Player = Player {
	_playerColour :: Colour,
	_playerCiv :: Civ,
	_playerGovernment :: Government,
	_playerTrade :: Trade,
	_playerCulture :: Culture,
	_playerCoins :: Coins,
	_playerTechs :: [TechCard]
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy 0 'base ''Player)
makeLenses ''Player

makePlayer colour civ = Player colour civ Anarchy (Trade 0) (Culture 0) (Coins 0) []

data GameState = Waiting | Running | Finished
	deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy 0 'base ''GameState)

newtype GameName = GameName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy 0 'base ''GameName)

data Game = Game {
	_gameCreator :: UserName,
	_gameState :: GameState,
	_gameBoardTiles :: Maybe [BoardTile],
	_gamePlayers :: Map.Map PlayerName Player
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''Game)
makeLenses ''Game

newGame :: UserName -> Game
newGame creator = Game creator Waiting Nothing Map.empty

type Games = Map.Map GameName Game

data CivState = CivState {
	_civGames :: Games
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''CivState)
makeLenses ''CivState

initialCivState :: CivState
initialCivState = CivState $ Map.fromList [
	(GameName "testgame",Game "public@thinking-machines.net" Running (Just [
		BoardTile (Tile Russia) (Coors 0 0) True Southward,
		BoardTile Tile1 (Coors 4 0) True Eastward,
		BoardTile Tile2 (Coors 0 4) True Southward,
		BoardTile Tile3 (Coors 4 4) False Southward,
		BoardTile Tile4 (Coors 0 8) False Southward,
		BoardTile Tile5 (Coors 4 8) True Northward,
		BoardTile Tile6 (Coors 0 12) True Westward,
		BoardTile (Tile America) (Coors 4 12) True Northward ])
		(Map.fromList [
			(PlayerName "Spieler Rot", Player Red Russia Despotism (Trade 1) (Culture 6) (Coins 1) [
				TechCard CodeOfLaws TechLevelI (Coins 2),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0),
				TechCard MonarchyTech TechLevelII (Coins 0) ]),
			(PlayerName "Spieler Blau", Player Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
				TechCard CodeOfLaws TechLevelI (Coins 1),
				TechCard HorsebackRiding TechLevelI (Coins 0),
				TechCard AnimalHusbandry TechLevelI (Coins 0),
				TechCard Philosophy TechLevelI (Coins 0),
				TechCard Navigation TechLevelI (Coins 0),
				TechCard Navy TechLevelI (Coins 0),
				TechCard MonarchyTech TechLevelII (Coins 0),
				TechCard PrintingPress TechLevelII (Coins 0),
				TechCard Sailing TechLevelII (Coins 0),
				TechCard Construction TechLevelII (Coins 0),
				TechCard Engineering TechLevelII (Coins 0),
				TechCard SteamPower TechLevelIII (Coins 0),
				TechCard Banking TechLevelIII (Coins 0),
				TechCard MilitaryScience TechLevelIII (Coins 0),
				TechCard Computers TechLevelIV (Coins 0),
				TechCard MassMedia TechLevelIV (Coins 0),
				TechCard SpaceFlight TechLevelV (Coins 0) ])
			])),

	(GameName "Testgame 2", Game "public@thinking-machines.net" Waiting Nothing
		(Map.fromList [
			(PlayerName "Spieler Blau", Player Blue America Democracy (Trade 0) (Culture 0) (Coins 0) [])
			])
		)
	]
