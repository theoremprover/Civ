module Version where

import Prelude

import Paths_Civ (version)
import Data.Version (showVersion)

dateString :: String
dateString = "Do 30. Apr 17:44:24 CEST 2015"

gitHash :: String
gitHash = "44ae07b"

cabalVersion :: String
cabalVersion = showVersion version