module Logic where

import Prelude

import System.Random
import Control.Monad
import Yesod.Core (MonadIO,liftIO)
import qualified Data.Map as Map

import Model
import TokenStack

rotate4x4coors orientation (Coors x y) = case orientation of
	Northward -> Coors x y
	Southward -> Coors (3-x) (3-y)
	Eastward  -> Coors (3-y) x
	Westward  -> Coors y (3-x)

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
