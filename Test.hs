{-# LANGUAGE
	CPP, DeriveDataTypeable, TupleSections, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, RecordWildCards, OverloadedStrings #-}

module Test where

createproduct :: [[a]] -> [a] -> [a] 
createproduct [] acc = acc
createproduct (l:ls) acc = createproduct ls $ concatMap (\ ac -> map (:ac) ls) acc
