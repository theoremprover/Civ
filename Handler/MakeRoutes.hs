{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Handler.MakeRoutes where

import Import

import Language.Haskell.TH

--putStrLn $(stringE . show =<< reify ''Bool)

makeRoutes :: Name -> String -> (String -> String) -> Q [Dec]
makeRoutes name funnamestr path = do
	TyConI (DataD _ _ _ constrs _) <- reify name
	funname <- newName funnamestr
	revealedname <- newName "revealed"
	valname <- newName "val"
	matches <- forM constrs $ \ (NormalC cname []) -> do
		let caseexpr = varE $ mkName $ path (nameBase cname)
		return $ Match (ConP cname []) (NormalB caseexpr) []
	return [ FunD funname [ Clause [VarP revealedname,VarP valname]
		(NormalB $ AppE (ConE 'StaticR) $ CaseE (VarE valname) matches) [] ] ]

toDial s = "_Dials_" ++ s ++ "_jpg"

toCulture s = "_Culture_" ++ s ++ "_jpg"

toTech s = "_Techs_" ++ s ++ "_jpg"

toBuilding s = "_Squares_" ++ s ++ "_jpg"
