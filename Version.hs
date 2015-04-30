module Version where

import Prelude

import Paths_Civ (version)
import Data.Version (showVersion)

dateString :: String
dateString = "Do 30. Apr 17:40:51 CEST 2015"

gitHash :: String
gitHash = "c6f8951"

cabalVersion :: String
cabalVersion = showVersion version