{-# LANGUAGE
	CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Model where

import Prelude
import Data.IxSet ( Indexable(..), IxSet(..), (@=), ixFun, ixSet, toAscList, Proxy(..) )
import Data.Acid
import Data.Data
import Data.Ord
import Data.Typeable
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Control.Monad.Reader

import Entities

import qualified Data.Ix as Ix

--newtype PlayerIndex = PlayerIndex Int deriving (Show,Read)
data Coors = Coors Int Int deriving (Show,Read,Data,Typeable)
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
	deriving (Show,Read,Eq,Data,Typeable)
$(deriveSafeCopy 0 'base ''Colour)

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Read,Eq,Data,Typeable)
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
	deriving (Show,Read,Data,Typeable)
$(deriveSafeCopy 0 'base ''Tech)

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable)
$(deriveSafeCopy 0 'base ''TechLevel)

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Read,Data,Typeable)
$(deriveSafeCopy 0 'base ''Government)

data BoardTile = BoardTile {
	boardTileId :: TileID,
	boardTileCoors :: Coors,
	boardTileOrientation :: Orientation,
	boardTileDiscovered :: Bool
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''BoardTile)

data TechCard = TechCard {
	techCardTechId :: Tech,
	techCardLevel :: TechLevel,
	techCardCoins :: Coins
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''TechCard)

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
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''Player)

instance Indexable Player where
	empty = ixSet
		[ ixFun $ \ player -> [ playerName player ] ]

data Game = Game {
	gameName :: String,
	gameBoardTiles :: [BoardTile],
	gamePlayers :: IxSet Player
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''Game)

instance Indexable Game where
	empty = ixSet
		[ ixFun $ \ game -> [ gameName game ] ]

data CivState = CivState {
	civGames :: IxSet Game
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''CivState)

--------------

getPlayerGame :: String -> String -> Query CivState (Player,Game)
getPlayerGame playername gamename = do
	CivState {..} <- ask
	let game = myGetOne (Data.IxSet.Proxy :: Data.IxSet.Proxy String) $ civGames @= gamename
	let player = myGetOne (Data.IxSet.Proxy :: Data.IxSet.Proxy String) $ (gamePlayers game) @= playername
	return (player,game)
	where
	myGetOne proxy = head . (toAscList proxy)

$(makeAcidic ''CivState ['getPlayerGame])

