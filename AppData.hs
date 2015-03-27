module AppData where

import Prelude
import Model

data AppData = AppData {
	appDataGameName :: String,
	appDataTiles :: [BoardTile]
	}
