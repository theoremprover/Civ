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

makeMultiRoutes :: [Name] -> String -> String -> String -> Q [Dec]
makeMultiRoutes names funnamestr -> pathpre pathpost = do
	constrss <- forM funames $ \ name -> do
		TyConI tok@(DataD _ _ _ constrs _) <- reify name
		return constrs
	funname <- newName funnamestr
	valname <- newName "val"
	
	
		