module Model where

import Prelude
import Data.IxSet
import Data.Acid
import Data.Data
import Entities

import qualified Data.Ix as Ix

data CivState = CivState {
	civGames :: IxSet Game
	}
	deriving (Data,Typeable,SafeCopy)

data Game = Game {
	gameName :: String,
	gameBoardTiles :: [BoardTile],
	gamePlayers :: [Player]
	}
	deriving (Data,Typeable,SafeCopy)
	
data Player = Player {
	playerName :: String,
	playerColour :: Colour,
	playerCiv :: Civ,
	playerGovernment :: Government,
	playerTrade :: Trade,
	playerCulture :: Culture,
	playerCoins :: Coins,
	playerTechs :: [TechCard]
	}
	deriving (Data,Typeable,SafeCopy)

data BoardTile = BoardTile {
	boardTileId :: TileID,
	boardTileCoors :: Coors,
	boardTileOrientation :: Orientation,
	boardTileDiscovered :: Bool
	}
	deriving (Data,Typeable,SafeCopy)

data TechCard = TechCard {
	techCardTechId :: Tech,
	techCardLevel :: TechLevel,
	techCardCoins :: Coins
	}
	deriving (Data,Typeable,SafeCopy)

--newtype PlayerIndex = PlayerIndex Int deriving (Show,Read)
data Coors = Coors Int Int deriving (Show,ReData,Typeable,SafeCopyad)

newtype Trade = Trade Int deriving (Show,Read,Num,Data,Typeable,SafeCopy)
newtype Coins = Coins Int deriving (Show,Read,Num,Data,Typeable,SafeCopy)
newtype Culture = Culture Int deriving (Show,Read,Num,Data,Typeable,SafeCopy)

data Orientation = Northward | Eastward | Southward | Westward
	deriving (Show,Read,Eq,Ord,Ix.Ix,Bounded,Data,Typeable,SafeCopy)

data Colour = Red | Green | Blue | Violet | Yellow
	deriving (Show,Read,Eq,Data,Typeable,SafeCopy)

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Read,Eq,Data,Typeable,SafeCopy)

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	Tile Civ
	deriving (Show,Read,Eq,Data,Typeable,SafeCopy)

data Phase = StartOfTurn | Trading | CityManagement | Movement | Research
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable,SafeCopy)

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	CivilService | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamPower | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplaceableParts | Flight | Plastics | Combustion | AtomicTheory |
	SpaceFlight
	deriving (Show,Read,Data,Typeable,SafeCopy)

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable,SafeCopy)

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Read,Data,Typeable,SafeCopy)
