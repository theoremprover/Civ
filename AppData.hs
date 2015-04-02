module AppData where

import Database.Persist.Types

import Prelude
import Model

data AppData = AppData {
	appDataGame :: Entity Game,
	appDataTiles :: [Entity BoardTile],
	appDataPlayers :: [Entity Player]
	}
