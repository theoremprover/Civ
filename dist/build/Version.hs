module Version where

import Prelude

import Paths_Civ (version)
import Data.Version (showVersion)

compilationDateString :: String
compilationDateString = "2015-12-04 01:38:01.995623"

gitHash :: String
gitHash = "'3507ebd'"

cabalVersion :: String
cabalVersion = showVersion version
