{-# LANGUAGE RecordWildCards #-}

module Actions where

import Prelude

import Model

data Action =
	BuildFirstCity
	deriving (Show,Eq,Ord)
	
phaseActions StartOfGame = [ BuildFirstCity ]
phaseActions _ = [ ]

