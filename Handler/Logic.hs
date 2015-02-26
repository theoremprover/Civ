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
pieceStackingLimit :: Player -> PieceLimit
pieceStackingLimit player = 4

playersPieces :: Game -> PlayerIndex -> [Piece]
playersPieces game playerindex = filter ((==playerindex).pieceOwner) $ gamePieces game

piecesInPlay :: PieceType -> Game -> PlayerIndex -> Int
piecesInPlay piecetype game playerindex = length $ filter ((==piecetype).pieceType) $ playersPieces game playerindex

playerUnused :: PieceType -> Game -> PlayerIndex -> Int
playerUnused piecetype game playerindex = playerMax piecetype game playerindex - piecesInPlay piecetype game playerindex

-- TODO: Implementieren
playerMax :: PieceType -> Game -> PlayerIndex -> Int
playerMax piecetype game playerindex = case piecetype of
	Wagon -> 2
	Flag  -> 6


