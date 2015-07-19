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

civStateLens = id
civGameLens gamename = civStateLens . civGames . at gamename

civPlayerLens gamename playername =
	civPlayersLens gamename . assocListLens playername

civPlayersLens gamename = civGameLens gamename . _Just . gamePlayers

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

