{-# LANGUAGE RankNTypes #-}

module Lenses where

import Prelude

import Control.Lens
import Control.Lens.Prism
import Control.Monad.State
import Control.Monad.Error (runErrorT,throwError)
import Data.Acid
import Data.Acid.Advanced

import Model

--Prism' [(key,val)] [(key,val)] (Maybe val)
--type Prism' s a = Prism s s a a 
--prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b Source
--prism' :: (b -> s) -> (s -> Maybe a) -> Prism s s a b 
assocListLens :: key -> Prism' [(key,val)] (Maybe val)
assocListLens key = prism' setter (lookup key) where
	setter :: 
	setter list Nothing = filter ((==key).fst) list
	setter [] (Just val) = [(key,val)]
	setter ((k,a):kas) (Just val) | k==key = (k,val) : kas
	setter (ka:kas) jval = ka : setter kas jval

civGameLens :: GameName -> Prism' CivState Game
civGameLens gamename = civGames . at gamename . _Just

civPlayerLens :: GameName -> PlayerName -> Prism' CivState (Maybe Player)
civPlayerLens gamename playername =
	civPlayersLens gamename . assocListLens playername

civPlayersLens :: GameName -> Prism' CivState Players
civPlayersLens gamename = civGameLens gamename . gamePlayers

updateCivLensU :: (val -> val) -> Prism' CivState val -> Update CivState () 
updateCivLensU fval lens = do
	modify (over lens fval)
	return ()

queryCivLensU :: Prism' CivState a -> Update CivState (Maybe a)
queryCivLensU lens = do
	civstate <- get
	return $ preview lens civstate

-------------- Conditions

checkCondition errmsg lens f = do
	civstate <- get
	case f (preview lens civstate) of
		False -> throwError errmsg
		True -> return ()

