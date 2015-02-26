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

-- TODO: Implementieren
unitLevel :: UnitCard -> Player -> Int
unitLevel unit player = 2

type PieceLimit = Int

-- TODO: Implementieren
pieceLimit :: Player -> PieceLimit
pieceLimit player = 4

playersPieces :: Game -> PlayerIndex -> [Piece]
playersPieces game playerindex = filter ((==playerindex).pieceOwner) $ gamePieces game

wagonsInPlay :: Game -> PlayerIndex -> Int
wagonsInPlay game playerindex = length $ playersPieces game playerindex

-- TODO: Implementieren
availableWagons :: Player -> Int
availableWagons player = 2

-- TODO: Implementieren
availableFlags :: Player -> Int
availableFlags player = 6
