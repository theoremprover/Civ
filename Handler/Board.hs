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

import Database.Persist.TH

import Import

import Handler.Board2

type PlayerIndex = Int
type XCoor = Int
type YCoor = Int
type CityID = Int
type Damage = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Games
    name String
    gameId GameId

Game
    boardTiles [BoardTile]
    playerSequence [Player]
    phase Phase
    playerTurn PlayerIndex

BoardTile
    tileID TileID
    xcoor XCoor
    ycoor YCoor
    discovered Bool
    orientation Orientation

Player
    name String
    civ Civ
    colour Colour
    freeCoins Int
    freeCulture Int
    dialTrade Int
    government Government
    policies [Policy]
    techTree [TechCard]
    units [UnitCard]
    cultureCards [CultureCard]
    greatPersons [GreatPersonCard]

TechCard
    treeLevel TechLevel
    tech Tech
    coins Int

GreatPersonCard
    person GreatPerson
    revealed Bool
    
CultureCard
    event CultureEvent
    revealed Bool

|]

{-

type FlagID = Int
type SettlerID = Int


data Player = Player {
	playerName   :: String,
	playerCiv    :: CivID,
	playerColour :: Colour }

data PlayerData = PlayerData {
	freeCoins :: Int,
	freeCulture :: Int,
	dialTrade :: Int,
--	techTree :: TechTree,
	playerGovernment :: Government,
	playerPolicies :: [Policy] }


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


data Piece = Flag FlagID | Settler SettlerID


type TechTree = Map TechLevel [Tech]

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