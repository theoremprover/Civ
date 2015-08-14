{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}

module Moves where

import Prelude

import Control.Monad.Error (runErrorT,ErrorT)
import Control.Lens hiding (Action)
import Control.Monad.State (modify,get,gets)
import Data.Acid
import Data.Acid.Advanced

import Logic
import Lenses
import TokenStack
import Model

type UpdateCivM a = ErrorT String (Update CivState) a

type UpdateResult = Either String ()

oK = Right ()
eRR errmsg = Left errmsg

runUpdateCivM :: UpdateCivM () -> Update CivState UpdateResult
runUpdateCivM = runErrorT

updateCivLensM :: (val -> val) -> Traversal' CivState val -> UpdateCivM () 
updateCivLensM fval lens = modify $ over lens fval

queryCivLensM :: Traversal' CivState a -> UpdateCivM (Maybe a)
queryCivLensM lens = do
	civstate <- Control.Monad.State.get
	return $ preview lens civstate

{-
erectBuilding :: GameName -> BuildingType -> Coors -> Owner -> (UpdateCivM Bool,UpdateCivM ())
erectBuilding gamename buildingtype coors owner = do
	Just () <- takeFromStackM (civGameLens gamename . _Just . gameBuildingStack) buildingtype
-}
--	City playername False False False NoWalls False mb_secondsquareori

buildCity :: GameName -> City -> UpdateCivM ()
buildCity gamename city = do
	updateCivLensM (const $ Just $ CityMarker city) $ civSquareLens gamename coors . squareTokenMarker
	case _cityMetropolisOri city of
		Nothing -> 