module AssocList where

import Prelude

import Data.Typeable
import Data.Data
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.List

import ModelVersion

data (Eq key) => AssocList key val = AssocList { fromAssocList :: [(key,val)] }
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''AssocList)

emptyAssocList = AssocList []

singletonAssocList :: (Eq key) => (key,val) -> AssocList key val
singletonAssocList (key,val) = addModifyAssoc key val undefined emptyAssocList

lookupAssocList :: (Eq key) => key -> AssocList key val -> Maybe val
lookupAssocList key assoclist = lookup key (fromAssocList assoclist)

nthAssocList :: (Eq key) => Int -> AssocList key val -> (key,val)
nthAssocList i assoclist = (fromAssocList assoclist)!!i

deleteAssoc :: (Eq key) => key -> AssocList key val -> AssocList key val
deleteAssoc key assoclist = AssocList $ filter ((/=key).fst) (fromAssocList assoclist)

mapAssoc :: (Eq key) => key -> (val -> val) -> AssocList key val -> AssocList key val 
mapAssoc key f assoclist = AssocList $ map mf (fromAssocList assoclist) where
	mf (k,v) = (k,case k==key of
		False -> v
		True  -> f v)

addModifyAssoc :: (Eq key) => key -> val -> (val -> val -> val) -> AssocList key val -> AssocList key val
addModifyAssoc k v f assoclist = case lookupAssocList k assoclist of
	Nothing  -> AssocList $ fromAssocList assoclist ++ [(k,v)]
	Just val -> mapAssoc k (const $ f val v) assoclist

concatAssocLists :: (Eq key) => (val -> val -> val) -> AssocList key val -> AssocList key val -> AssocList key val
concatAssocLists f al1 al2 = foldl (\ al (k,v) -> addModifyAssoc k v f al) al1 (fromAssocList al2)