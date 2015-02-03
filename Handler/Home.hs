{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        let tileSize :: Int = 175
        let tiles = makeBoard tileSize
        setTitle "Civ"
        $(widgetFile "homepage")

makeBoard tilesize = map calcpos [
    (StaticR board_Brett01_gif,(0::Int,0::Int)),(StaticR board_Brett02_gif,(4,0)),(StaticR board_Brett01_gif,(8,0)),(StaticR board_Brett02_gif,(12,0)),
    (StaticR board_Brett01_gif,(0,4)),(StaticR board_Brett01_gif,(4,4)),(StaticR board_Brett02_gif,(8,4)),(StaticR board_Brett02_gif,(12,4)),
    (StaticR board_Brett02_gif,(0,8)),(StaticR board_Brett01_gif,(4,8)),(StaticR board_Brett02_gif,(8,8)),(StaticR board_Brett01_gif,(12,8)),
    (StaticR board_Brett02_gif,(0,12)),(StaticR board_Brett02_gif,(4,12)),(StaticR board_Brett01_gif,(8,12)),(StaticR board_Brett02_gif,(12,12)) ] 
    where
    calcpos (t,(x,y)) = (t,(div (x*tilesize) 4,div (y*tilesize) 4))