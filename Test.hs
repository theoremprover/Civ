{-# LANGUAGE
	CPP, DeriveDataTypeable, TupleSections, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, RecordWildCards, OverloadedStrings #-}

module Test where

import Control.Lens
import Control.Lens.Traversal
import Control.Applicative
import Data.Foldable
--import Control.Exception.Lens
import Control.Exception

data Test a = Test1 { _test11 :: a, _test12 :: Maybe (Test a) }
	deriving (Eq,Show)

makeLenses ''Test

--foldr :: (a -> b -> b) -> b -> t a -> b
instance Foldable Test where
	foldr f z (Test1 a Nothing) = f a z
	foldr f z (Test1 a (Just test)) = f a (Data.Foldable.foldr f z test)

instance Functor Test where
	fmap f (Test1 a Nothing) = Test1 (f a) Nothing
	fmap f (Test1 a (Just test)) = Test1 (f a) $ Just $ fmap f test

{-
traverse :: Applicative f =>   (a -> f b) -> t a -> f (t b)
(<*>)    ::                  f (a -> b)   -> f a -> f b
(<$>)    :: Functor f     =>   (a -> b)   -> f a -> f b 
-}

instance Traversable Test where
	traverse f (Test1 a Nothing) = Test1 <$> (f a) <*> pure Nothing
 	traverse f (Test1 a (Just test)) = Test1 <$> (f a) <*> (Just <$> traverse f test)

test1 = Test1 3 (Just $ Test1 4 Nothing)

--t = toListOf test11 test1

{-
data Test a = Test1 {
	_test11 :: Maybe a,
	_test12 :: Maybe (Test a) } |
	Test2 { _test2 :: Test a }
	deriving (Show,Eq)

instance Traversable Test where
	traverse f (Test1 mb_int tests) = Test1 <$> (f mb_int) <*> (traverse f tests)
	traverse f (Test2 test) = Test2 <$> traverse f test

--test3 = Test1 (Just 3) [ Test2 (Test1 Nothing []),Test1 (Just 4) [], Test1 (Just 5) [] ]
test3 = Test1 (Just 3) (Test2 (Test1 Nothing Nothing))

-}

data A = A { _aint :: Int }
	deriving (Eq,Show)
makeLenses ''A
data T = T { _ts :: Maybe A }
	deriving (Eq,Show)
makeLenses ''T

--data NotJustException = NotJustException String
--instance Exception NotJustException

_fromJust :: String -> Prism' (Maybe a) a
_fromJust errmsg = prism' Just (\ mb_a -> case mb_a of
	Just a -> Just a
	Nothing -> error errmsg)

t = do
	let a = preview (ts . (_fromJust "myerr") . aint) (T (Nothing))
	print a
	`catch` (\ ex@(SomeException _) -> print ex)
