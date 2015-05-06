module GameMonad where

import Import

import Model
import DisplayData


getGameAcid :: Handler Game
getGameAcid = do
	

{-
selectFromIds field ids = runDB $ selectList [ field <-. ids ] []

loadAppData gameid = do
	[game@(Entity _ (Game appDataGameName tileids playerids))] <- selectFromIds GameId [gameid]
	tiles <- selectFromIds BoardTileId tileids
	players <- selectFromIds PlayerId playerids
	return $ AppData game tiles players

loadDisplayData playerid displayscale = do
	return $ DisplayData playerid displayscale
-}
