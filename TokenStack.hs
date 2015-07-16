module TokenStack where

import Prelude
import qualified Data.Map as Map

type TokenStack tokenty token = Map.Map tokenty [token]

tokenStackFromList :: (Ord key) => [(key,val)] -> Map.Map key val
tokenStackFromList l = Map.fromList l

takeFromStack :: (Ord toktyp) => toktyp -> TokenStack toktyp tok -> Maybe (tok,TokenStack toktyp tok)
takeFromStack toktyp stack = case Map.lookup toktyp stack of
	Just (l:ls) -> Just (l,Map.adjust tail toktyp stack)
	_ -> Nothing

putOnStack :: (Ord toktyp) => toktyp -> tok -> TokenStack toktyp tok -> TokenStack toktyp tok
putOnStack toktyp tok stack = Map.insertWith (++) toktyp [tok] stack

{-
initialCityStack = tokenStackFromList $ replicateUnit [
	(CityT,3),(MetropolisT,1) ]
-> [ (CityT,[(),(),()]),
-}
replicateUnit :: [(toktyp,Int)] -> [(toktyp,[()])]
replicateUnit = map $ \ (tt,n) -> (tt,replicate n ())

{-
initialHutStack :: TokenStack () Hut
initialHutStack = tokenStackFromList $ replicateToken [
	(ResourceHut Spy,6),(ResourceHut Wheat,7),(ResourceHut Incense,6),
-> [((),[ResourceHut Spy,ResourceHut Wheat,...)]
-}
replicateToken :: [(tok,Int)] -> [((),[tok])]
replicateToken l = [ ( (), concatMap (\ (t,n) -> replicate n t) l ) ]

