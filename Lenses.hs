{-# LANGUAGE RankNTypes #-}

module Lenses where

import Prelude

import Control.Lens
import Control.Monad.State
import Control.Monad.Error (runErrorT,throwError)
import Data.Acid
import Data.Acid.Advanced

import Model

assocListLens key = lens (lookup key) setter where
	setter list Nothing = filter ((==key).fst) list
	setter [] (Just val) = [(key,val)]
	setter ((k,a):kas) (Just val) | k==key = (k,val) : kas
	setter (ka:kas) jval = ka : setter kas jval

civGameLens :: GameName -> Lens' CivState (Maybe Game)
civGameLens gamename = civGames . at gamename

civPlayerLens :: GameName -> PlayerName -> Lens' CivState (Maybe Player)
civPlayerLens gamename playername =
	civPlayersLens gamename . assocListLens playername

civPlayersLens :: GameName -> Lens' CivState (Maybe Players)
civPlayersLens gamename = civGameLens gamename . gamePlayers

updateCivLensU fval lens = do
	modify (over lens fval)
	return ()

queryCivLensU :: Lens' CivState a -> Update CivState a
queryCivLensU lens = do
	civstate <- get
	return $ view lens civstate

-------------- Conditions

checkCondition errmsg lens f = do
	civstate <- get
	case f (preview lens civstate) of
		False -> throwError errmsg
		True -> return ()

