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
neighbourSquares coors = [ coors +/+ Coors dx dy | dx <- [-1,1], dy <- [-1,1] ]

coorDistance :: Coors -> Coors -> Int
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

data AssocList key val = AssocList { fromAssocList :: [(key,val)] }
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''AssocList)

lookupAssocList :: (Eq key) => key -> AssocList key val -> Maybe val
lookupAssocList key assoclist = lookup key (fromAssocList assoclist)

nthAssocList :: (Eq key) => Int -> AssocList key val -> (key,val)
nthAssocList i assoclist = (fromAssocList assoclist)!!i

addAssoc :: (key,val) -> AssocList key val -> AssocList key val
addAssoc assoc assoclist = assoclist { fromAssocList = assoc:(fromAssocList assoclist) }

deleteAssoc :: (key,val) -> AssocList key val -> AssocList key val
deleteAssoc assoc assoclist = assoclist { fromAssocList = delete assoc (fromAssocList assoclist) }

mapAssoc :: key -> (val -> val) -> AssocList key val 
mapAssoc key f assoclist = assoclist { fromAssocList = map mf (fromAssocList assoclist) } where
	mf (k,v) = case k==key of
		False -> (k,v)
		True  -> (k,f v)
