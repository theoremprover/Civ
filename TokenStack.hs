module TokenStack where

import Prelude
import qualified Data.Map as Map

type TokenStack tokenty token = Map.Map tokenty [token]

tokenStackFromList :: [(key,val)] -> Map.Map key val
tokenStackFromList l = Map.fromList l

takeFromStack :: toktyp -> TokenStack toktyp tok -> Maybe (tok,TokenStack toktyp tok)
takeFromStack toktyp stack = case Map.lookup toktyp stack of
	Just (l:ls) = Just (l,Map.adjust tail toktyp stack)
	_ -> Nothing

putOnStack :: toktyp -> tok -> TokenStack toktyp tok -> TokenStack toktyp tok
putOnStack toktyp tok stack = Map.insertWith (++) toktyp [tok] stack
