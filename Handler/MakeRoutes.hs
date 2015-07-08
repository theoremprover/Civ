{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Handler.MakeRoutes where

import Import

import Language.Haskell.TH


makeRoutes :: Name -> String -> (String -> Bool -> String) -> Q [Dec]
makeRoutes name funnamestr path = do
	TyConI (DataD _ _ _ constrs _) <- reify name
	funname <- newName funnamestr
	revealedname <- newName "revealed"
	valname <- newName "val"
	matches <- forM constrs $ \ (NormalC cname []) -> do
		let revname  = mkName $ path (nameBase cname) True
		let backname = mkName $ path (nameBase cname) False
		caseexpr <- [e| if $(varE revealedname) then $(varE revname) else $(varE backname) |]
		return $ Match (ConP cname []) (NormalB caseexpr) []
	return [ FunD funname [ Clause [VarP revealedname,VarP valname]
		(NormalB $ AppE (ConE 'StaticR) $ CaseE (VarE valname) matches) [] ] ]

toDial s _ = "_Dials_" ++ s ++ "_jpg"

toCulture s True = "_Culture_" ++ s ++ "_jpg"
