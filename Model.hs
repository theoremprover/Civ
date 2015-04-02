module Model (
	module Model,
	module Model2
	) where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model2

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable
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

PlayerAction
    
|]
