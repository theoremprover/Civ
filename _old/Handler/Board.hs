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
import qualified Prelude

import Handler.Board2

type PlayerIndex = Int
type XCoor = Int
type YCoor = Int
type CityID = Int
type Damage = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Game
	name String
    boardTiles [BoardTileId]
    playerSequence [PlayerId]
    phase Phase
    playerTurn PlayerIndex
	startPlayer PlayerIndex
    pieces [PieceId]

Piece
    xcoor XCoor
    ycoor YCoor
    owner PlayerIndex
    type PieceType
    deriving Ord
    deriving Eq

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
    techTree [TechCardId]
    units [UnitCardId]
    cultureCards [CultureCardId]
    greatPersons [GreatPersonCardId]
    resources [Resource]
    huts [Hut]
    villages [Village]
    cities [CityId]

City
    xCoor XCoor
    yCoor YCoor
    capital Bool
    type CityType
    orientation Orientation
    walls Bool
    assignedSettlers [PieceId]

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
