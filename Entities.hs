module Entities where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model2

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    email Text
    password Text Maybe
    verkey Text Maybe
    verified Bool
    game GameId Maybe
    player PlayerId Maybe
    UniqueUser email
    deriving Typeable
    deriving Show

Email
    email Text
    user UserId Maybe
    verkey Text Maybe
    UniqueEmail email

Game
    name String
    boardTiles [BoardTileId]
    players [PlayerId]
    UniqueGameName name

BoardTile
    tileID TileID
    xcoor XCoor
    ycoor YCoor
    discovered Bool
    orientation Orientation

Player
    name String
    colour Colour
    civ Civ
	trade Trade
	UniquePlayerName name
	deriving Show
|]
