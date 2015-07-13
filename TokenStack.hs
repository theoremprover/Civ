module TokenStack where

import Prelude
import qualified Data.Map as Map

type TokenStack tokenty token = Map.Map tokenty [token]

tokenStackFromList l = Map.fromList l

takeFromStack toktyp stack = (Map.adjust
	where
	e = Map.