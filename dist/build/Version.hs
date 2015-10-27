module Version where

import Prelude

import Paths_Civ (version)
import Data.Version (showVersion)

compilationDateString :: String
compilationDateString = "2015-10-27 01:23:26.992308"

gitHash :: String
gitHash = "'913e7d3'"

cabalVersion :: String
cabalVersion = showVersion version
