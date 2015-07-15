module Logic where

import Model
import System.Random
import Control.Monad
import Data.Map

initialCivState :: IO CivState
initialCivState = do
--	now <- getCurrentTime
	return $ CivState $ Map.fromList []
{-
		let tiles = [
			BoardTile (Tile Russia) (Coors 0 0) True Southward,
			BoardTile Tile1 (Coors 4 0) True Eastward,
			BoardTile Tile2 (Coors 0 4) True Southward,
			BoardTile Tile3 (Coors 4 4) False Southward,
			BoardTile Tile4 (Coors 0 8) False Southward,
			BoardTile Tile5 (Coors 4 8) True Northward,
			BoardTile Tile6 (Coors 0 12) True Westward,
			BoardTile (Tile America) (Coors 4 12) True Northward ] in
			(GameName "testgame",Game now "public@thinking-machines.net" Running
				tiles
				[(PlayerName "Spieler Rot", Player "public@thinking-machines.net" Red Russia Despotism (Trade 1) (Culture 6) (Coins 1) [
						TechCard CodeOfLaws TechLevelI (Coins 2),
						TechCard HorsebackRiding TechLevelI (Coins 0),
						TechCard AnimalHusbandry TechLevelI (Coins 0),
						TechCard Philosophy TechLevelI (Coins 0),
						TechCard Navigation TechLevelI (Coins 0),
						TechCard Navy TechLevelI (Coins 0),
						TechCard MonarchyTech TechLevelII (Coins 0) ]),
					(PlayerName "Spieler Blau", Player "reitmeier@thinking-machines.net" Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
						TechCard CodeOfLaws TechLevelI (Coins 1),
						TechCard HorsebackRiding TechLevelI (Coins 0),
						TechCard AnimalHusbandry TechLevelI (Coins 0),
						TechCard Philosophy TechLevelI (Coins 0),
						TechCard Navigation TechLevelI (Coins 0),
						TechCard Navy TechLevelI (Coins 0),
						TechCard MonarchyTech TechLevelII (Coins 0),
						TechCard PrintingPress TechLevelII (Coins 0),
						TechCard Sailing TechLevelII (Coins 0),
						TechCard Construction TechLevelII (Coins 0),
						TechCard Engineering TechLevelII (Coins 0),
						TechCard SteamEngine TechLevelIII (Coins 0),
						TechCard Banking TechLevelIII (Coins 0),
						TechCard MilitaryScience TechLevelIII (Coins 0),
						TechCard Computers TechLevelIV (Coins 0),
						TechCard MassMedia TechLevelIV (Coins 0),
						TechCard SpaceFlight TechLevelV (Coins 0) ])
					]
				(createBoard tiles))
		]
-}

rotate4x4coors orientation (Coors x y) = case orientation of
	Northward -> Coors x y
	Southward -> Coors (3-x) (3-y)
	Eastward  -> Coors (3-y) x
	Westward  -> Coors y (3-x)

shuffle :: (MonadIO m) => TokenStack a b -> m (TokenStack a b)
shuffle tokenstack = do
	ss <- forM (Map.toList tokenstack) $ \ (key,l) -> do
		l' <- shuffleList l
		return (key,l')
	return $ Map.fromList ss

shuffleList :: (MonadIO m) => [a] -> [a]
shuffleList l = shufflelist' l []
	where
	shufflelist' [] acc = return acc
	shufflelist' ls acc = do
		i <- liftIO $ randomRIO (0,length ls - 1)
		shufflelist' (take i ls ++ drop (i+1) ls) ((ls!!i) : acc)
