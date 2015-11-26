module AssocList where

import Prelude

import Data.Typeable
import Data.Data
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.List

import ModelVersion

data AssocList key val = AssocList { fromAssocList :: [(key,val)] }
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''AssocList)

emptyAssocList = AssocList []

singletonAssocList :: (Eq key) => (key,val) -> AssocList key val
singletonAssocList keyval = addAssoc keyval emptyAssocList

lookupAssocList :: (Eq key) => key -> AssocList key val -> Maybe val
lookupAssocList key assoclist = lookup key (fromAssocList assoclist)

nthAssocList :: (Eq key) => Int -> AssocList key val -> (key,val)
nthAssocList i assoclist = (fromAssocList assoclist)!!i

addAssoc :: (key,val) -> AssocList key val -> AssocList key val
addAssoc assoc assoclist = assoclist { fromAssocList = fromAssocList assoclist ++ [assoc] }

deleteAssoc :: (Eq key,Eq val) => (key,val) -> AssocList key val -> AssocList key val
deleteAssoc assoc assoclist = assoclist { fromAssocList = delete assoc (fromAssocList assoclist) }

mapAssoc :: (Eq key) => key -> (val -> val) -> AssocList key val -> AssocList key val 
mapAssoc key f assoclist = assoclist { fromAssocList = map mf (fromAssocList assoclist) } where
	mf (k,v) = (k,case k==key of
		False -> v
		True  -> f v)

addModifyAssoc :: (Eq key) => key -> val -> (val -> val -> val) -> AssocList key val -> AssocList key val
addModifyAssoc k v f assoclist = case lookupAssocList k assoclist of
	Nothing  -> addAssoc (k,v) assoclist
	Just val -> mapAssoc k (f val) assoclist

concatAssocLists :: (Eq key) => AssocList key val -> AssocList key val -> AssocList key val
concatAssocLists al1 al2 = AssocList (fromAssocList al1 ++ fromAssocList al2)