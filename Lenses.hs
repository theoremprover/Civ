{-# LANGUAGE RankNTypes,TypeSynonymInstances,TypeFamilies,FlexibleInstances #-}

module Lenses where

import Prelude

import Control.Lens
import Control.Lens.Prism
import Control.Lens.At
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error (runErrorT,throwError)
import Data.Acid
import Data.Acid.Advanced

import Model

--Prism' [(key,val)] (Maybe val)
--type Prism' s a = Prism s s a a 
--prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b

{-
assocListLens :: key -> [(key,val)] -> Prism' [(key,val)] val
assocListLens key = prism' setter (lookup key) where
	setter :: [(key,val)] -> Maybe val -> [(key,val)]
	setter list        Nothing             = filter ((==key).fst) list
	setter []          (Just val)          = [(key,val)]
	setter ((k,a):kas) (Just val) | k==key = (k,val) : kas
	setter (ka:kas)    jval                = ka : setter kas jval
-}

type instance Index   (AssocList key val) = key
type instance IxValue (AssocList key val) = val

instance (Eq key) => Ixed (AssocList key val) where
	ix key f al@(AssocList l) = case Prelude.lookup key l of
		Just val -> fmap (\ val' -> AssocList $ replacewith val' l) $ f val
		Nothing  -> pure al
		where
		replacewith _ [] = []
		replacewith val' ((k,v):ls) | k==key = (k,val') : replacewith val' ls
		replacewith val' ((k,v):ls) = (k,v) : replacewith val' ls

instance (Eq key) => At (AssocList key val)

assocListLens :: (Eq key) => key -> Lens' (AssocList key val) (Maybe val)
assocListLens key = at key
		
--civGameLens :: GameName -> Prism' CivState Game
civGameLens gamename = civGames . at gamename . _Just

--civPlayerLens :: GameName -> PlayerName -> Prism' CivState Player
civPlayerLens gamename playername = civPlayersLens gamename . assocListLens playername . _Just . _2

--civPlayersLens :: GameName -> Lens' CivState Players
civPlayersLens gamename = civGameLens gamename . gamePlayers

updateCivLensU :: (val -> val) -> Prism' CivState val -> Update CivState () 
updateCivLensU fval lens = do
	modify (over lens fval)
	return ()

queryCivLensU :: Prism' CivState a -> Update CivState (Maybe a)
queryCivLensU lens = do
	civstate <- get
	return $ preview lens civstate
