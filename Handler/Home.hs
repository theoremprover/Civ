{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Import

import Handler.Board
import Handler.BoardDisplay

import qualified Data.Map.Strict as Map

getHomeR :: Handler Html
getHomeR = do
--    persons <- runDB $ selectList ([] :: [Filter Person]) []
    defaultLayout $ do
        setTitle "Civ"
        aDomId <- newIdent
        let game = testGame
        let squaresize = 80 :: Int
        $(widgetFile "homepage")

---------

testGame :: Game
testGame = Game
	(Map.fromList [
		((0,0),BoardTile True Tile1 Northward),
		((4,0),BoardTile False Tile2 Northward),
		((8,0),BoardTile True Tile9 Westward),
		((12,0),BoardTile True (StartTile America) Westward),
		((0,4),BoardTile True (StartTile Russia) Eastward),
		((4,4),BoardTile True Tile14 Eastward),
		((8,4),BoardTile False Tile25 Northward),
		((12,4),BoardTile True Tile26 Southward) ])

{-
shuffle l = getStdGen >>= shuffle' l where
	shuffle' l gen = (l!!i) : shuffle' (take i l ++ drop (i+1) l) gen' where
		(i,gen') = randomR (0,length l - 1) gen
-}

{-
make4PlayerBoard tilesize = do
shuffeledtiles <- shuffle $ map StaticR [tiles_Tile1_jpg .. tiles_Tile19_jpg]
	return $ take 12 shuffeledtiles
    (StaticR board_Brett01_gif,(0::Int,0::Int)),(StaticR board_Brett02_gif,(4,0)),(StaticR board_Brett01_gif,(8,0)),(StaticR board_Brett02_gif,(12,0)),
    (StaticR board_Brett01_gif,(0,4)),(StaticR board_Brett01_gif,(4,4)),(StaticR board_Brett02_gif,(8,4)),(StaticR board_Brett02_gif,(12,4)),
    (StaticR board_Brett02_gif,(0,8)),(StaticR board_Brett01_gif,(4,8)),(StaticR board_Brett02_gif,(8,8)),(StaticR board_Brett01_gif,(12,8)),
    (StaticR board_Brett02_gif,(0,12)),(StaticR board_Brett02_gif,(4,12)),(StaticR board_Brett01_gif,(8,12)),(StaticR board_Brett02_gif,(12,12)) ] 
-}