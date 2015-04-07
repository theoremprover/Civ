module ExecutePlayerAction where

import Import
import Model

executePlayerAction :: PlayerAction -> Handler ()
executePlayerAction playeraction = do
	return ()