module Handler.BoardDisplay where

import Import

import Handler.Board

import Text.Printf

tile2StaticR (BoardTile discovered tileid _) = StaticR $ case tileid of
	StartTile America  -> if discovered then _Images_Tiles_America_front_jpg else _Images_Tiles_America_back_jpg
	StartTile Arabs    -> if discovered then _Images_Tiles_Arabs_front_jpg else _Images_Tiles_Arabs_back_jpg
	StartTile Aztecs   -> if discovered then _Images_Tiles_Aztecs_front_jpg else _Images_Tiles_Aztecs_back_jpg
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
	Tile1  -> if discovered then _Images_Tiles_Tile1_jpg else _Images_Tiles_Back_jpg
	Tile2  -> if discovered then _Images_Tiles_Tile2_jpg else _Images_Tiles_Back_jpg
	Tile3  -> if discovered then _Images_Tiles_Tile3_jpg else _Images_Tiles_Back_jpg
	Tile4  -> if discovered then _Images_Tiles_Tile4_jpg else _Images_Tiles_Back_jpg
	Tile5  -> if discovered then _Images_Tiles_Tile5_jpg else _Images_Tiles_Back_jpg
	Tile6  -> if discovered then _Images_Tiles_Tile6_jpg else _Images_Tiles_Back_jpg
	Tile7  -> if discovered then _Images_Tiles_Tile7_jpg else _Images_Tiles_Back_jpg
	Tile8  -> if discovered then _Images_Tiles_Tile8_jpg else _Images_Tiles_Back_jpg
	Tile9  -> if discovered then _Images_Tiles_Tile9_jpg else _Images_Tiles_Back_jpg
	Tile10 -> if discovered then _Images_Tiles_Tile10_jpg else _Images_Tiles_Back_jpg
	Tile11 -> if discovered then _Images_Tiles_Tile11_jpg else _Images_Tiles_Back_jpg
	Tile12 -> if discovered then _Images_Tiles_Tile12_jpg else _Images_Tiles_Back_jpg
	Tile13 -> if discovered then _Images_Tiles_Tile13_jpg else _Images_Tiles_Back_jpg
	Tile14 -> if discovered then _Images_Tiles_Tile14_jpg else _Images_Tiles_Back_jpg
	Tile15 -> if discovered then _Images_Tiles_Tile15_jpg else _Images_Tiles_Back_jpg
	Tile16 -> if discovered then _Images_Tiles_Tile16_jpg else _Images_Tiles_Back_jpg
	Tile17 -> if discovered then _Images_Tiles_Tile17_jpg else _Images_Tiles_Back_jpg
	Tile18 -> if discovered then _Images_Tiles_Tile18_jpg else _Images_Tiles_Back_jpg
	Tile19 -> if discovered then _Images_Tiles_Tile19_jpg else _Images_Tiles_Back_jpg
	Tile20 -> if discovered then _Images_Tiles_Tile20_jpg else _Images_Tiles_Back_jpg
	Tile21 -> if discovered then _Images_Tiles_Tile21_jpg else _Images_Tiles_Back_jpg
	Tile22 -> if discovered then _Images_Tiles_Tile22_jpg else _Images_Tiles_Back_jpg
	Tile23 -> if discovered then _Images_Tiles_Tile23_jpg else _Images_Tiles_Back_jpg
	Tile24 -> if discovered then _Images_Tiles_Tile24_jpg else _Images_Tiles_Back_jpg
	Tile25 -> if discovered then _Images_Tiles_Tile25_jpg else _Images_Tiles_Back_jpg
	Tile26 -> if discovered then _Images_Tiles_Tile26_jpg else _Images_Tiles_Back_jpg
	Tile27 -> if discovered then _Images_Tiles_Tile27_jpg else _Images_Tiles_Back_jpg

tile2class :: BoardTile -> String
tile2class (BoardTile _ _ orientation) =
	"tilesize " ++ case orientation of
	Northward -> "northward"
	Southward -> "southward"
	Eastward  -> "eastward"
	Westward  -> "westward"

tile2style :: Int -> Coors -> String
tile2style squaresize (x,y) =
	printf "position:absolute; left:%ipx; top:%ipx;" (x*squaresize) (y*squaresize)
