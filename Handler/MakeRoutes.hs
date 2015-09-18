{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Handler.MakeRoutes where

import Import

--import Data.List

import Language.Haskell.TH

makeMultiRoutes :: [Name] -> String -> [ String ] -> Q [Dec]
makeMultiRoutes names funnamestr pathintersperses = do
	cnamess <- forM names getconstrname
	funname <- newName funnamestr
	valname <- newName "val"
	matches <- forM (createxproduct cnamess [[]]) $ \ combcnames -> do
		let imagevar = VarE $ mkName $ blend pathintersperses
			(map (\ (cname,args) -> nameBase cname ++ concatMap (("_"++).nameBase) args) combcnames)
		return $ Match (TupP [ ConP cname (map (\ a -> ConP a []) args) |
			(cname,args) <- combcnames ]) (NormalB imagevar) []
	return [ FunD funname [ Clause [VarP valname]
		(NormalB $ AppE (ConE 'StaticR) $ CaseE (VarE valname) matches) [] ] ]		

	where

	blend :: [String] -> [String] -> String
	blend [i] [] = i
	blend (i:is) (c:cs) = i ++ c ++ blend is cs
	blend is cs = error $ "blend: is=" ++ show is ++ " and cs=" ++ show cs ++ " have not the right length"

	createxproduct :: [[a]] -> [[a]] -> [[a]] 
	createxproduct [] acc = acc
	createxproduct (l:ls) acc = createxproduct ls $ concat [ map (++[e]) acc | e <- l ]
	
	getconstrname :: Name -> Q [(Name,[Name])]
	getconstrname name = do
		TyConI (DataD _ _ _ constrs _) <- reify name
		ns <- forM constrs $ \ constr -> case constr of
			NormalC cname [] -> return [(cname,[])]
			NormalC cname [(_,ConT name')] -> do
				cs <- getconstrname name'
				forM cs $ \ (c,[]) -> return (cname,[c])
		return $ concat ns