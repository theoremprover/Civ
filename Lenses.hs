{-# LANGUAGE RankNTypes,TypeSynonymInstances,TypeFamilies,FlexibleInstances #-}

module Lenses where

import Prelude

import Control.Lens
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

instance (Eq key) => Ixed (AssocList key val)

instance (Eq key) => At (AssocList key val) where
	at key = lens ((Prelude.lookup key).fromAssocList) setter
		where
		setter (AssocList l) mb_val = AssocList $ replacewith l mb_val
		replacewith [] Nothing    = []
		replacewith [] (Just val) = [(key,val)]
		replacewith ((k,v):ls) Nothing     | k==key = ls
		replacewith ((k,v):ls) (Just val') | k==key = (k,val') : ls
		replacewith (l:ls) mb_val = l : replacewith ls mb_val

assocListLens :: (Eq key) => key -> Lens' (AssocList key val) (Maybe val)
assocListLens key = at key

civGameLens :: GameName -> Lens' CivState (Maybe Game)
civGameLens gamename = civGames . at gamename

civPlayerLens :: GameName -> PlayerName -> Traversal' CivState (Maybe Player)
civPlayerLens gamename playername = civPlayersLens gamename . assocListLens playername

civPlayersLens :: GameName -> Traversal' CivState Players
civPlayersLens gamename = civGameLens gamename . _Just . gamePlayers

civPlayerIndexLens :: GameName -> Int -> Traversal' CivState (PlayerName,Player)
civPlayerIndexLens gamename index = civPlayersLens gamename . nthAssocListLens index

nthAssocListLens :: Int -> Lens' (AssocList key val) (key,val)
nthAssocListLens index = lens ((!!index).fromAssocList) ins where
	ins (AssocList l) e = AssocList $ take index l ++ [e] ++ drop index l
