{-# LANGUAGE TemplateHaskell #-}

module Model (
	module Entities,
	module Model2,
	module Model
	)where

import Database.Persist.TH

import Prelude

import Entities
import Model2

data PlayerAction =
	ChangeTrade PlayerId Trade Trade
	deriving (Show,Read,Eq)
derivePersistField "PlayerAction"
