module Logic where

import Prelude

import System.Random
import Control.Monad
import Yesod.Core (MonadIO,liftIO)
import qualified Data.Map as Map
import Data.List

import Model
import TokenStack

rotate4x4coors orientation (Coors x y) = case orientation of
	Northward -> Coors    x     y
	Southward -> Coors (3-x) (3-y)
	Eastward  -> Coors (3-y)    x
	Westward  -> Coors    y  (3-x)

surroundingSquares :: Coor -> Coors -> [ Coors ]
surroundingSquares radius coors = delete coors
	[ coors +/+ Coors x y | x <- [-radius..radius], y <- [-radius..radius] ]

neighbourSquares :: Coors -> [ Coors ]
neighbourSquares coors = [ coors +/+ Coors dx dy | (dx,dy) <- [(-1,0),(1,0),(0,-1),(0,1)] ]

coorDistance :: Coors -> Coors -> Coor
coorDistance (Coors x1 y1) (Coors x2 y2) = max (abs $ x2-x1) (abs $ y2-y1)

outskirtsOf :: [ Coors ] -> [ Coors ]
outskirtsOf coorss = nub $ concatMap (surroundingSquares 1) coorss \\ coorss

addCoorsOri :: Coors -> Orientation -> Coors
addCoorsOri (Coors x y) ori = case ori of
	Northward -> Coors  x    (y-1)
	Eastward  -> Coors (x+1)  y
	Southward -> Coors  x    (y+1)
	Westward  -> Coors (x-1)  y

addOri :: Orientation -> Orientation -> Orientation
addOri o1 o2 = toEnum $ mod (fromEnum o1 + fromEnum o2) 4

coorDiffOri :: Coors -> Coors -> Orientation
coorDiffOri c1@(Coors x1 y1) c2@(Coors x2 y2) = case (x1==x2,y1==y2) of
	(True,False) | y1<y2 -> Southward 
	(True,False) | y1>y2 -> Northward 
	(False,True) | x1<x2 -> Eastward 
	(False,True) | x1>x2 -> Westward 
	_ -> error $ "Cannot calculate coorDiffOri " ++ show c1 ++ " " ++ show c2 

shuffle :: (Ord a,MonadIO m) => TokenStack a b -> m (TokenStack a b)
shuffle tokenstack = do
	ss <- forM (Map.toList tokenstack) $ \ (key,l) -> do
		l' <- shuffleList l
		return (key,l')
	return $ Map.fromList ss

shuffleList :: (MonadIO m) => [a] -> m [a]
shuffleList l = shufflelist' l []
	where
	shufflelist' [] acc = return acc
	shufflelist' ls acc = do
		i <- liftIO $ randomRIO (0,length ls - 1)
		shufflelist' (take i ls ++ drop (i+1) ls) ((ls!!i) : acc)

