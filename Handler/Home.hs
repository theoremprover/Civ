module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        let tileSize = 125 :: Int
        setTitle "Civ"
        $(widgetFile "homepage")
