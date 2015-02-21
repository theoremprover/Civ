module Handler.Logic where

import Prelude

import Handler.Board
import Handler.Board2

basicCityDefence :: City -> Int
basicCityDefence city =
	(if cityWalls city then 4 else 0)
	+
	(case cityType city of
		PlainCity -> 6
		Metropolis -> 8)
	+
	(if cityCapital city then 6 else 0)

unitLevel :: UnitCard -> Player -> Int
unitLevel unit player = 2