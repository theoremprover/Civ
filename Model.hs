module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model2

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Game
	name String
    boardTiles [BoardTileId]

BoardTile
    tileID TileID
    xcoor XCoor
    ycoor YCoor
    discovered Bool
    orientation Orientation

|]
