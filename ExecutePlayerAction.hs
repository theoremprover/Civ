module ExecutePlayerAction where

import Database.Persist.Sql(fromSqlKey,toSqlKey)

import Import
import Model

executePlayerAction :: PlayerAction -> Handler ()
executePlayerAction playeraction = do
	runDB $ case playeraction of
		ChangeTrade playerid fromtrade totrade ->
			update (toSqlKey playerid :: Key Player) [ PlayerTrade =. totrade ]
	return ()