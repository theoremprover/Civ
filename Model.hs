{-# LANGUAGE
	CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Model where

import Prelude
import Data.Acid
import Data.Data
import Data.Text (Text(..))
--import Data.Ord
import Data.Typeable
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Control.Monad.Reader
import Data.List

import Entities

import qualified Data.Ix as Ix

--newtype PlayerIndex = PlayerIndex Int deriving (Show,Read)
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
	boardTileId :: TileID,
	boardTileCoors :: Coors,
	boardTileDiscovered :: Bool,
	boardTileOrientation :: Orientation
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy 0 'base ''BoardTile)

data TechCard = TechCard {
	techCardTechId :: Tech,
	techCardLevel :: TechLevel,
	techCardCoins :: Coins
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy 0 'base ''TechCard)

newtype PlayerName = PlayerName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy 0 'base ''PlayerName)

data Player = Player {
	playerName :: PlayerName,
	playerColour :: Colour,
	playerCiv :: Civ,
	playerGovernment :: Government,
	playerTrade :: Trade,
	playerCulture :: Culture,
	playerCoins :: Coins,
	playerTechs :: [TechCard]
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy 0 'base ''Player)

{-
instance Ord Player where
	compare = comparing playerName
instance Eq Player where
	p1 == p2 = playerName p1 == playerName p2
-}

data GameState = Waiting Int | Running | Finished
	deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy 0 'base ''GameState)

newtype GameName = GameName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy 0 'base ''GameName)

data Game = Game {
	gameName :: GameName,
	gameState :: GameState,
	gameBoardTiles :: [BoardTile],
	gamePlayers :: [Player]
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''Game)

{-
instance Ord Game where
	compare = comparing gameName
instance Eq Game where
	g1 == g2 = gameName g1 == gameName g2
-}

data CivState = CivState {
	civGames :: [Game]
	}
	deriving (Data,Typeable)
$(deriveSafeCopy 0 'base ''CivState)

--------------

getGames :: Query CivState [Game]
getGames = do
	CivState {..} <- ask
	return civGames

getPlayerGame :: Text -> Text -> Query CivState (Maybe (Player,Game))
getPlayerGame playername gamename = do
	CivState {..} <- ask
	let Just game = find ((==(GameName gamename)).gameName) civGames
	let Just player = find ((==(PlayerName playername)).playerName) $ gamePlayers game
	return (player,game)

incTrade :: Text -> Text -> Trade -> Update CivState ()
incTrade playername gamename trade = do
	-- hier die Lens
	return ()

$(makeAcidic ''CivState [
	'getGames,
	'getPlayerGame,
	'incTrade])

