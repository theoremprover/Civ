module Handler.Logic where

import Prelude

import Handler.Board
import Handler.Board2


unitLevel :: UnitCard -> Player -> Int
unitLevel unit player = 2