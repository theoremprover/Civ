{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Board where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Import

import Handler.Board2

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Games
    name Text
    gameId GameId

Game
    boardTiles [BoardTile]

BoardTile
    tileID TileID
    xcoor Int
    ycoor Int
    discovered Bool
    orientation Orientation

|]

{-
data CivID =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving Show

type PlayerID = Int

data Game = Game {
	gameBoard      :: Map Coors BoardTile,
	gamePlayers    :: [PlayerID] }   -- First player is start player
	gamePhase      :: Phase,
	gamePlayerTurn :: PlayerIndex,
	gamePlayerData :: Map PlayerID PlayerData

type Tiles = Map TileID Tile

data Tile = Tile {
	tileSquares :: Map Coors Square }

data Settlement = Hut Bool | BarbarianVillage Bool

data Square = Square {
	squareTerrain :: Terrain,
	squareCity :: Maybe (PlayerID,CityID),
	squareUnits :: [(PlayerID,Piece)],
	squareResource :: Maybe Resource,
	squareCulture :: Int,
	squareHutOrVillage :: Maybe Settlement,
	squareCoin :: Int }

tiles :: Tiles
tiles = Map.fromList [
	(Tile1,Tile $ Map.fromList [
		((0,0),ds Desert),((0,0),ds Desert),((0,0),hut ds Grass),((0,0),ds Grass),
	where
	ds terrain = Square terrain Nothing [] Nothing 0 0
	hut f terrain = f terrain { squareHutOrVillage :: Just (Left False) }
	vil f terrain = f terrain { squareHutOrVillage :: Just (Right False) }

type FlagID = Int
type SettlerID = Int

data Piece = Flag FlagID | Settler SettlerID

type CityID = Int

data Terrain = Grass | Woods | Sea | Desert | Mountain

data Resource = Incense | Iron | Cloth | Wheat | Atom | Spy

data Colour = Red | Green | Blue | Violet | Yellow

data Player = Player {
	playerName   :: String,
	playerCiv    :: CivID,
	playerColour :: Colour }

type PlayerIndex = Int

data Phase = StartOfTurn | Trade | CityManagement | Movement | Research
	deriving (Show,Eq,Ord)

data PlayerData = PlayerData {
	freeCoins :: Int,
	freeCulture :: Int,
	dialTrade :: Int,
--	techTree :: TechTree,
	playerGovernment :: Government,
	playerPolicies :: [Policy] }

data Policy =
	Rationalism | NaturalReligion | MilitaryTradition | UrbanDevelopment |
	Patronage | Pacifism | OrganizedReligion | Expansionism
	deriving (Show,Eq)

type TechLevel = Int

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	CivilService | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamPower | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplaceableParts | Flight | Plastics | Combustion | AtomicTheory |
	SpaceFlight
	deriving (Show,Eq)

data Building =
	Market | Bank | Temple | Cathedral | Granary | Aquaeduct | Library | University |
	Barracks | Academy | TradingPost | Workshop | IronMine | Harbour | Shipyard

data Unit =
	Infantry_1_3 | Infantry_2_2 | Infantry_3_1 |
	Artillery_1_3 | Artillery_2_2 | Artillery_3_1 |
	Cavalry_1_3 | Cavalry_2_2 | Cavalry_3_1 |
	Aircraft_5_7 | Aircraft_6_6 | Aircraft_7_5

data Wonder =
	Stonehenge | Colossus | HangingGardens | TheOracle | TheGreatWall |
	ChichenItza | Pyramids | GreatLighthouse | StatueOfZeus |
	AngkorWat | HimejiCastle | TajMahal | PorcelainTower | MachuPichu |
	BrandenburgGate | Louvre | NotreDame | LeonardosWorkshop |
	SydneyOperaHouse | StatueOfLiberty | PanamaCanal | UnitedNations |
	BigBen | CristoRedentor | Kremlin | Pentagon | TheInternet
	deriving (Show,Eq)

data CityState = CityState1 | CityState2 | CityState3 | CityState4 | CityState5

type TechTree = Map TechLevel [Tech]

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Eq)

class Outskirts a where
	oCoins :: Int
	oHammers :: Int
	oTrade :: Int
	oCulture :: Int
	

class Abilities a where
	aCoins :: Maybe Int
	aCultureStartTurn :: Maybe Int
	aCultureOutskirts :: Maybe Int
	aHammersOutskirts :: Maybe Int
	aTradeOutskirts
	aTradeStartTurn :: Maybe Int
	aEnablesBuildings :: Maybe Building
	aEnablesWonder :: Maybe Wonder
	aEnablesGovernment :: Maybe Government
	aMilitaryAdvantage :: Maybe Int
	aPieceLimit :: Maybe Int
	aPieceLimitIncrease :: Maybe Int
-}