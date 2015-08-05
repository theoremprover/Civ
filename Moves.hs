{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}

module Moves where

import Prelude

import Data.Maybe

import Acidic
import Logic
import Lenses
import TokenStack
import Model


type UpdateCivM a = ErrorT String (Update CivState) a

erectBuilding :: GameName -> BuildingType -> Coors -> Owner -> (UpdateCivM Bool,UpdateCivM ())
erectBuilding gamename buildingtype coors owner = do
	Just () <- takeFromStackM (civGameLens gamename . _Just . gameBuildingStack) buildingtype
	