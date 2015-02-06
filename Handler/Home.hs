{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Import

import Random

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        let tileSize :: Int = 175
        board <- make4PlayerBoard tileSize
        setTitle "Civ"
        $(widgetFile "homepage")

{-
shuffle l = getStdGen >>= shuffle' l where
	shuffle' l gen = (l!!i) : shuffle' (take i l ++ drop (i+1) l) gen' where
		(i,gen') = randomR (0,length l - 1) gen
-}

make4PlayerBoard tilesize = do
{-
shuffeledtiles <- shuffle $ map StaticR [tiles_Tile1_jpg .. tiles_Tile19_jpg]
	return $ take 12 shuffeledtiles
    (StaticR board_Brett01_gif,(0::Int,0::Int)),(StaticR board_Brett02_gif,(4,0)),(StaticR board_Brett01_gif,(8,0)),(StaticR board_Brett02_gif,(12,0)),
    (StaticR board_Brett01_gif,(0,4)),(StaticR board_Brett01_gif,(4,4)),(StaticR board_Brett02_gif,(8,4)),(StaticR board_Brett02_gif,(12,4)),
    (StaticR board_Brett02_gif,(0,8)),(StaticR board_Brett01_gif,(4,8)),(StaticR board_Brett02_gif,(8,8)),(StaticR board_Brett01_gif,(12,8)),
    (StaticR board_Brett02_gif,(0,12)),(StaticR board_Brett02_gif,(4,12)),(StaticR board_Brett01_gif,(8,12)),(StaticR board_Brett02_gif,(12,12)) ] 
-}