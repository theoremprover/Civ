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

makeMultiRoutes :: [Name] -> String -> [ String ] -> Q [Dec]
makeMultiRoutes names funnamestr pathintersperses = do
	cnamess <- forM names $ \ name -> do
		TyConI tok@(DataD _ _ _ constrs _) <- reify name
		return $ map (\ (NormalC cname []) -> cname) constrs
	funname <- newName funnamestr
	valname <- newName "val"
	matches <- forM (createxproduct cnamess [[]]) $ \ combcnames -> do
		let imagevar = VarE $ mkName $
			concatMap (\ combcnames -> map nameBase ) combcnamess
		return $ Match (TupP [ ConP cname [] | cname <- combcnames ]) (NormalB imagevar) []
	return [ FunD funname [ Clause [VarP valname]
		(NormalB $ AppE (ConE 'StaticR) $ CaseE (VarE valname) matches) [] ] ]		

	where

	blend :: [String] -> [String] -> String

	createxproduct :: [[a]] -> [[a]] -> [[a]] 
	createxproduct [] acc = acc
	createxproduct (l:ls) acc = createxproduct ls $ concat [ map (e:) acc | e <- l ]
	
		