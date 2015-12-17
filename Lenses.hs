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

import AssocList
import Model

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

civCityLens :: GameName -> Coors -> Traversal' CivState City
civCityLens gamename coors = civSquareLens gamename coors . squareTokenMarker . _Just . cityMarker

civPlayerIndexLens :: GameName -> Int -> Traversal' CivState (PlayerName,Player)
civPlayerIndexLens gamename index = civPlayersLens gamename . nthAssocListLens index

nthAssocListLens :: (Eq key) => Int -> Lens' (AssocList key val) (key,val)
nthAssocListLens index = lens ((!!index).fromAssocList) ins where
	ins (AssocList l) e = AssocList $ take index l ++ [e] ++ drop index l

civSquareLens :: GameName -> Coors -> Traversal' CivState Square
civSquareLens gamename coors = civGameLens gamename . _Just . gameBoard . ix coors

civRandomGenLens :: GameName -> Lens' CivState RandomGen