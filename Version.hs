module Version where

import Prelude

import Paths_Civ (version)
import Data.Version (showVersion)

dateString :: String
dateString = "Mo 27. Apr 18:54:03 CEST 2015"

gitHash :: String
gitHash = "<GITHASH>"

cabalVersion :: String
cabalVersion = showVersion version