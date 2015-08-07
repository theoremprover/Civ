{-# LANGUAGE
	CPP, DeriveDataTypeable, TupleSections, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, RecordWildCards, OverloadedStrings #-}

module Test where

createxproduct :: [[a]] -> [[a]] -> [[a]] 
createxproduct [] acc = acc
createxproduct (l:ls) acc = createxproduct ls $ concat [ map (e:) acc | e <- l ]
