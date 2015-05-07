module GameMonad where

import Import

import Model
import DisplayData


getGame :: User -> Handler Game
getGameAcid user = do
	App {..} <- getYesod
	 appCivAcid
	
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
