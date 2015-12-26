module Version where

import Prelude

import Paths_Civ (version)
import Data.Version (showVersion)

compilationDateString :: String
compilationDateString = "2015-12-11 02:17:51.207263"

gitHash :: String
gitHash = "'4e78e44'"

cabalVersion :: String
cabalVersion = showVersion version
