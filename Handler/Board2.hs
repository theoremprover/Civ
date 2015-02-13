{-# LANGUAGE TemplateHaskell            #-}

module Handler.Board2 where

import Import

import Database.Persist.TH

data Orientation = Northward | Southward | Eastward | Westward
	deriving (Show,Read,Eq)
derivePersistField "Orientation"

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	TileAmerica | TileArabs | TileAztecs | TileChina | TileEgypt | TileEnglish | TileFrench | 
	TileGermany | TileGreeks | TileIndians | TileJapanese | TileMongols | TileRome | 
	TileRussia | TileSpanish | TileZulu
	deriving (Show,Read,Eq)
derivePersistField "TileID"
