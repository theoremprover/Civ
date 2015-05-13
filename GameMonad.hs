{-# LANGUAGE TemplateHaskell #-}

module GameMonad where

import Data.Aeson.TH

import Import

import Model
import Data.Acid
import Data.Acid.Advanced

data GameAdminAction =
	CreateGame |
	JoinGame

$(deriveJSON id ''GameAdminAction)

getGamePlayer :: (GameName,PlayerName) -> Handler (Maybe (Game,Player))
getGamePlayer (gamename,playername) = do
	App {..} <- getYesod
	games <- query' appCivAcid GetGames
	case filter ((==gamename).gameName) games of
		[game] -> case filter ((==playername).playerName) (gamePlayers game) of
			[player] -> return $ Just (game,player)
			_ -> return Nothing
		_ -> return Nothing


{-
getGame :: User -> Handler Game
getGameAcid user = do
	App {..} <- getYesod
	 appCivAcid
	
selectFromIds field ids = runDB $ selectList [ field <-. ids ] []

loadAppData gameid = do
	[game@(Entity _ (Game appDataGameName tileids playerids))] <- selectFromIds GameId [gameid]
	tiles <- selectFromIds BoardTileId tileids
	players <- selectFromIds PlayerId playerids
	return $ AppData game tiles players

loadDisplayData playerid displayscale = do
	return $ DisplayData playerid displayscale
-}
