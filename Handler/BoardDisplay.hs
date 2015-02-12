module Handler.BoardDisplay where

import Import

import Handler.Board

tile2StaticR (BoardTile discovered tileid _) = StaticR $ case tileid of
	StartTile America  -> if discovered then _Images_Tiles_America_front_jpg else _Images_Tiles_America_back_jpg
	StartTile Arabs    -> if discovered then _Images_Tiles_Arabs_front_jpg else _Images_Tiles_Arabs_back_jpg
	StartTile Aztec    -> if discovered then _Images_Tiles_Aztecs_front_jpg else _Images_Tiles_Aztecs_back_jpg
	StartTile China    -> if discovered then _Images_Tiles_China_front_jpg else _Images_Tiles_China_back_jpg
	StartTile Egypt    -> if discovered then _Images_Tiles_Egypt_front_jpg else _Images_Tiles_Egypt_back_jpg
	StartTile English  -> if discovered then _Images_Tiles_English_front_jpg else _Images_Tiles_English_back_jpg
	StartTile French   -> if discovered then _Images_Tiles_French_front_jpg else _Images_Tiles_French_back_jpg
	StartTile Germany  -> if discovered then _Images_Tiles_Germany_front_jpg else _Images_Tiles_Germany_back_jpg
	StartTile Greeks   -> if discovered then _Images_Tiles_Greeks_front_jpg else _Images_Tiles_Greeks_back_jpg
	StartTile Indians  -> if discovered then _Images_Tiles_Indians_front_jpg else _Images_Tiles_Indians_back_jpg
	StartTile Japanese -> if discovered then _Images_Tiles_Japanese_front_jpg else _Images_Tiles_Japanese_back_jpg
	StartTile Mongols  -> if discovered then _Images_Tiles_Mongols_front_jpg else _Images_Tiles_Mongols_back_jpg
	StartTile Rome     -> if discovered then _Images_Tiles_Rome_front_jpg else _Images_Tiles_Rome_back_jpg
	StartTile Russia   -> if discovered then _Images_Tiles_Russia_front_jpg else _Images_Tiles_Russia_back_jpg
	StartTile Spanish  -> if discovered then _Images_Tiles_Spanish_front_jpg else _Images_Tiles_Spanish_back_jpg
	StartTile Zulu     -> if discovered then _Images_Tiles_Zulu_front_jpg else _Images_Tiles_Zulu_back_jpg
	_ -> if not discovered then _Images_Tiles_Back_jpg else case tileid of
		Tile1  -> _Images_Tiles_Tile1_jpg
		Tile2  -> _Images_Tiles_Tile2_jpg
		Tile3  -> _Images_Tiles_Tile3_jpg
		Tile4  -> _Images_Tiles_Tile4_jpg
		Tile5  -> _Images_Tiles_Tile5_jpg
		Tile6  -> _Images_Tiles_Tile6_jpg
		Tile7  -> _Images_Tiles_Tile7_jpg
		Tile8  -> _Images_Tiles_Tile8_jpg
		Tile9  -> _Images_Tiles_Tile9_jpg
		Tile10 -> _Images_Tiles_Tile10_jpg
		Tile11 -> _Images_Tiles_Tile11_jpg
		Tile12 -> _Images_Tiles_Tile12_jpg
		Tile13 -> _Images_Tiles_Tile13_jpg
		Tile14 -> _Images_Tiles_Tile14_jpg
		Tile15 -> _Images_Tiles_Tile15_jpg
		Tile16 -> _Images_Tiles_Tile16_jpg
		Tile17 -> _Images_Tiles_Tile17_jpg
		Tile18 -> _Images_Tiles_Tile18_jpg
		Tile19 -> _Images_Tiles_Tile19_jpg
		Tile20 -> _Images_Tiles_Tile20_jpg
		Tile21 -> _Images_Tiles_Tile21_jpg
		Tile22 -> _Images_Tiles_Tile22_jpg
		Tile23 -> _Images_Tiles_Tile23_jpg
		Tile24 -> _Images_Tiles_Tile24_jpg
		Tile25 -> _Images_Tiles_Tile25_jpg
		Tile26 -> _Images_Tiles_Tile26_jpg
		Tile27 -> _Images_Tiles_Tile27_jpg

coor2class (x,y) (BoardTile _ _ orientation) =
	"tilesize coorx" ++ show x ++ " coory" ++ show y ++ " " ++ case orientation of
		Northward -> "northward"
		Southward -> "southward"
		Eastward -> "eastward"
		Westward -> "westward"