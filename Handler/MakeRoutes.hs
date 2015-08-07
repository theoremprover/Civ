{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Handler.MakeRoutes where

import Import

import Language.Haskell.TH

--putStrLn $(stringE . show =<< reify ''Bool)

makeRoutes :: Name -> String -> String -> String -> Q [Dec]
makeRoutes name funnamestr pathpre pathpost = do
	TyConI tok@(DataD _ _ _ constrs _) <- reify name
	funname <- newName funnamestr
	valname <- newName "val"
	matches <- forM constrs $ \ (NormalC cname []) -> do
		let caseexpr = VarE $ mkName $ pathpre ++ nameBase cname ++ pathpost
		return $ Match (ConP cname []) (NormalB caseexpr) []
	return [ FunD funname [ Clause [VarP valname]
		(NormalB $ AppE (ConE 'StaticR) $ CaseE (VarE valname) matches) [] ] ]

makeMultiRoutes :: [Name] -> [String] -> String -> String -> Q [Dec]
makeMultiRoutes names funnamestrs pathpre pathpost = do
	cfs <- forM (zip names funnamestrs) $ \ (name,funname) -> do
		TyConI tok@(DataD _ _ _ constrs _) <- reify name
		let cnames = map (\ (NormalC cname []) -> pathpre ++ nameBase cname ++ pathpost) constrs
	funname <- newName funnamestr
	valname <- newName "val"
	matches <- forM cfs $ \ constrs -> do
		let caseexpr = VarE $ mkName $ 
		return $ Match (TupP [ ConP cname [] | cname <- cnames ]) (NormalB caseexpr) []
	return [ FunD funname [ Clause [VarP valname]
		(NormalB $ AppE (ConE 'StaticR) $ CaseE (VarE valname) matches) [] ] ]		

	where

	createxproduct :: [[a]] -> [[a]] -> [[a]] 
	createxproduct [] acc = acc
	createxproduct (l:ls) acc = createxproduct ls $ concat [ map (e:) acc | e <- l ]
	
		