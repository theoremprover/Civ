module AppData where

import Model

data AppData = AppData {
	appDataGameName :: String,
	appDataTiles :: [BoardTile]
	}
