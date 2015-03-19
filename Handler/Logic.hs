module Handler.Logic where

import Prelude

import Handler.Board
import Handler.Board2

basicCityDefence :: City -> Int
basicCityDefence city =
	(case cityType city of
		PlainCity -> 6
		Metropolis -> 8)
	+
	(if cityCapital city then 6 else 0)
	+
	(if cityWalls city then 4 else 0)

-- TODO: Implementieren
unitLevel :: UnitCard -> Player -> Int
unitLevel unit player = 2

-- TODO: Implementieren
pieceStackingLimit :: Player -> Int
pieceStackingLimit player = 4

playersPieces :: Game -> PlayerIndex -> [Piece]
playersPieces game playerindex = filter ((==playerindex) . pieceOwner) $ gamePieces game

piecesInPlay :: PieceType -> Game -> PlayerIndex -> Int
piecesInPlay piecetype game playerindex = length $ filter ((==piecetype).pieceType) $ playersPieces game playerindex

playerUnused :: PieceType -> Game -> PlayerIndex -> Int
playerUnused piecetype game playerindex = playerMax piecetype game playerindex - piecesInPlay piecetype game playerindex

-- TODO: Implementieren
playerMax :: PieceType -> Game -> PlayerIndex -> Int
playerMax piecetype game playerindex = case piecetype of
	Wagon -> 2 --getAbility game playerindex AvailableWagons
	Flag  -> 6 --getAbility game playerindex AvailableFlags

-----------

{-
data Ability = 

infix 0 +=

class Ability a b where
	(+=) :: a -> (AbilityValue b -> AbilityValue b) -> Ability a b

instance Ability MilBranch UnitLevel where
	milbranch += f = 

class HasAbilities x where
	get :: x -> [ Ability ]


instance HasAbilities Civ where
	get Russia = [ Available Flag += (+1) ]

data AbilityID = StackingLimit | AvailableWagons | AvailableFlags | UnitLevel MilBranch | Coins
	deriving (Show,Eq)

-- IM CODE (GameM):
--	numWagons ::  <- get playerid 
--	unitLevelArtillery <- get playerid :: Artillery

ability MetalCasting = [ UnitLevel Artillery := Enabled 3 ]

-}

data Action = Action {
	actionPhase :: Phase,
	actionDescription :: String,
	actionAction :: GameM () }

data Value a = Disabled | Change (a -> a) | Enabled a

data Values a where =
	UnitLevelV :: MilBranch  

instance HasAbilities Player where
	ability player = [
		StartOfTurn != "Change Government" (startOfTurn player),
		Trade != "Obtain Trade" (obtainTrade player),
		CityManagement != "City Management" (cityManagement player),
		Movement != "Movement" (movement player),
		Research != "Research" (research player) ]



instance HasAbilities Tech where
	get Logistics = [
		Infantry  := Enabled UnitLevelII,
		Cavalry   := Enabled UnitLevelII,
		Artillery := Enabled UnitLevelII ]
	get Bureaucracy = [
		Coins += (+1) ]
	get x = error $ "get not implemented for " ++ show x
