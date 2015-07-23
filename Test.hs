{-# LANGUAGE
	CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Test where

import Control.Lens
import Control.Lens.Traversal
import Control.Applicative

data Test a = Test1 { _test11 :: a, _test12 :: Maybe (Test a) }
	deriving (Eq,Show)

t = Test1 3 (Just $ Test1 4 Nothing)

{-
traverse :: Applicative f =>   (a -> f b) -> t a -> f (t b)
(<*>)    ::                  f (a -> b)   -> f a -> f b
(<$>)    :: Functor f     =>   (a -> b)   -> f a -> f b 
-}
instance Traversable Test where
	traverse f (Test1 a Nothing) = Test1 <$> (f a) <*> pure Nothing
	traverse f (Test1 a (Just test)) = Test1 <$> (f a) <*> traverse f test

{-
data Test a = Test1 {
	_test11 :: Maybe a,
	_test12 :: Maybe (Test a) } |
	Test2 { _test2 :: Test a }
	deriving (Show,Eq)

makeLenses ''Test

instance Traversable Test where
	traverse f (Test1 mb_int tests) = Test1 <$> (f mb_int) <*> (traverse f tests)
	traverse f (Test2 test) = Test2 <$> traverse f test

--test3 = Test1 (Just 3) [ Test2 (Test1 Nothing []),Test1 (Just 4) [], Test1 (Just 5) [] ]
test3 = Test1 (Just 3) (Test2 (Test1 Nothing Nothing))

t = toListOf test11 test3
-}