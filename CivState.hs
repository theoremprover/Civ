module CivState where

import Prelude
import Data.IxSet
import Data.Acid
import Data.Data
import Model
import Entities

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
	

data TechCard = TechCard {
	techCardTechId :: Tech,
	techCardLevel :: TechLevel,
	techCardCoins :: Coins
	}
	deriving (Data,Typeable,SafeCopy)


{-
Game
    name String
    boardTiles [BoardTileId]
    players [PlayerId]
    UniqueGameName name

BoardTile
    tileID TileID
    coors Coors
    discovered Bool
    orientation Orientation

Player
    name String
    colour Colour
    civ Civ
    government Government
	trade Trade
    culture Culture
    coins Coins
    techs [TechCardId]
	UniquePlayerName name
	deriving Show

TechCard
    tech Tech
    level TechLevel
    coins Coins
    deriving Show
-}
