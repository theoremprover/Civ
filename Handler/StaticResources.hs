{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Handler.StaticResources where

import Import

import Handler.MakeRoutes

import Model

boardTileRoute boardtile = case _boardTileId boardtile of
	Tile civ -> case civ of
		America  -> if revealed then _Tiles_TileAmerica_front_jpg else _Tiles_TileAmerica_back_jpg
		Arabs    -> if revealed then _Tiles_TileArabs_front_jpg else _Tiles_TileArabs_back_jpg
		Aztecs   -> if revealed then _Tiles_TileAztecs_front_jpg else _Tiles_TileAztecs_back_jpg
		China    -> if revealed then _Tiles_TileChina_front_jpg else _Tiles_TileChina_back_jpg
		Egypt    -> if revealed then _Tiles_TileEgypt_front_jpg else _Tiles_TileEgypt_back_jpg
		English  -> if revealed then _Tiles_TileEnglish_front_jpg else _Tiles_TileEnglish_back_jpg
		French   -> if revealed then _Tiles_TileFrench_front_jpg else _Tiles_TileFrench_back_jpg
		Germany  -> if revealed then _Tiles_TileGermany_front_jpg else _Tiles_TileGermany_back_jpg
		Greeks   -> if revealed then _Tiles_TileGreeks_front_jpg else _Tiles_TileGreeks_back_jpg
		Indians  -> if revealed then _Tiles_TileIndians_front_jpg else _Tiles_TileIndians_back_jpg
		Japanese -> if revealed then _Tiles_TileJapanese_front_jpg else _Tiles_TileJapanese_back_jpg
		Mongols  -> if revealed then _Tiles_TileMongols_front_jpg else _Tiles_TileMongols_back_jpg
		Rome     -> if revealed then _Tiles_TileRome_front_jpg else _Tiles_TileRome_back_jpg
		Russia   -> if revealed then _Tiles_TileRussia_front_jpg else _Tiles_TileRussia_back_jpg
		Spanish  -> if revealed then _Tiles_TileSpanish_front_jpg else _Tiles_TileSpanish_back_jpg
		Zulu     -> if revealed then _Tiles_TileZulu_front_jpg else _Tiles_TileZulu_back_jpg
	_ | not revealed -> _Tiles_Back_jpg
	Tile1 -> _Tiles_Tile1_jpg
	Tile2 -> _Tiles_Tile2_jpg
	Tile3 -> _Tiles_Tile3_jpg
	Tile4 -> _Tiles_Tile4_jpg
	Tile5 -> _Tiles_Tile5_jpg
	Tile6 -> _Tiles_Tile6_jpg
	Tile7 -> _Tiles_Tile7_jpg
	Tile8 -> _Tiles_Tile8_jpg
	Tile9 -> _Tiles_Tile9_jpg
	Tile10 -> _Tiles_Tile10_jpg
	Tile11 -> _Tiles_Tile11_jpg
	Tile12 -> _Tiles_Tile12_jpg
	Tile13 -> _Tiles_Tile13_jpg
	Tile14 -> _Tiles_Tile14_jpg
	Tile15 -> _Tiles_Tile15_jpg
	Tile16 -> _Tiles_Tile16_jpg
	Tile17 -> _Tiles_Tile17_jpg
	Tile18 -> _Tiles_Tile18_jpg
	Tile19 -> _Tiles_Tile19_jpg
	Tile20 -> _Tiles_Tile20_jpg
	Tile21 -> _Tiles_Tile21_jpg
	Tile22 -> _Tiles_Tile22_jpg
	Tile23 -> _Tiles_Tile23_jpg
	Tile24 -> _Tiles_Tile24_jpg
	Tile25 -> _Tiles_Tile25_jpg
	Tile26 -> _Tiles_Tile26_jpg
	Tile27 -> _Tiles_Tile27_jpg
	where
	revealed = _boardTileDiscovered boardtile

$(makeRoutes ''Civ "dialRoute" toDial)
tradeDialRoute = _Dials_Tradedial_gif
coinDialRoute = _Dials_Coindial_gif
oneCultureRoute = _Dials_1Culture_gif
fiveCultureRoute = _Dials_5Culture_gif
coinRoute = _Dials_Coin_gif

$(makeRoutes ''CultureCardID "cultureRouteRevealed" toCulture)
cultureRoute culturecard = 

--toCulture level _ False = "_Culture_" ++ show level ++ "_back_jpg"

--putStrLn $(stringE . show =<< reify ''Bool)

{-
makeBoardTileRoutes :: Q [Dec]
makeBoardTileRoutes = do
	TyConI (DataD _ _ _ tileconstrs) <- reify ''TileID
	revealedname <- newName "revealed"
	cases <- forM tileconstrs $ \case
		NormalC cname [] -> do
			let revname = mkName $ "_Tiles_" ++ nameBase cname ++ "_jpg"
			let backname = mkName "_Tiles_Back_jpg"
			return [e|if $(revealedname) then $(revname) else $(backname)|]
		NormalC cname [(_,ConT)] -> do
			let revname = mkName $ "_Tiles_" ++ nameBase cname ++ "_front_jpg"
			let backname = mkName "_Tiles_Back_jpg"
			return [e|if $(revealedname) then $(revname) else $(backname)|]
	[d|
boardTileRoute :: TileID -> Bool -> Route Static
boardTileRoute tileid $(revealedname) = case tileid of $(map tomatch tileconstrs)
|]
	where
	tomatch (NormalC cname []) -> cond ("_Tiles_Tile") ()

	pbs = 
	cases = [ Match (ConP pname []) (NormalB bodyexpr) | (pname,bodyexpr) <- pbs ]

	NormalC cname cargs -> return $ case cargs of
		[] -> Match (ConP pname []) (NormalB bodyexpr)

case tileid of
	Tile25     -> if revealed then _Tiles_Tile25_jpg    else _Tiles_Back_jpg
	Tile Arabs -> if revealed then _Tiles_TileArabs_front_jpg else _Tiles_TileArabs_back_jpg
-}