{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.BoardDisplay where

import Import
import qualified Prelude

import Text.Hamlet (HtmlUrl, hamlet)
--import Data.Text (Text)
--import Text.Blaze.Html.Renderer.String (renderHtml)

import Handler.Board
import Handler.Board2

import Text.Printf
--import Text.Regex.Base
import Text.Regex.TDFA
import Data.Typeable
import Data.List((!!))

type Coor = Double
type Size = (Coor,Coor)
type Pos = (Coor,Coor)
type Proportion = (Double,Double)

data DisplayInfo = DisplayInfo {
	scaleCoor :: Coor -> String,
	squareSize :: Size,
	tileSize :: Size,
	vertCardSize :: Size,
	horCardSize :: Size,
	dialSize :: Size,
	dialNaveProp :: Proportion,
	tradeDialSize :: Size,
	coinDialSize :: Size,
	boardSize :: Size,
	techCardCoinProp :: Proportion,
	coinSize :: Size,
	coinDistanceProp :: Proportion,
	dialCoinProp :: Proportion,
	governmentProp :: Proportion,
	oneCultureSize :: Size,
	oneCultureDistProp :: Proportion,
	fiveCultureSize :: Size,
	fiveCultureDistProp :: Proportion,
	cultureRowsProp :: Proportion,
	cultureRowsDistProp :: Proportion
	}

defaultDisplayInfo = DisplayInfo {
	scaleCoor     = undefined,
	squareSize    = scalex  93 1.00,
	tileSize      = scalex 372 1.00,
	vertCardSize  = scalex 122 1.54,
	horCardSize   = scalex 187 0.65,
	dialSize      = scalex 561 0.65,
	dialNaveProp  = (0.754,0.355),
	tradeDialSize = scalex 168 1.40,
	coinDialSize  = scalex  83 1.73,
	boardSize     = undefined,
	techCardCoinProp = (0.24,0.40),
	coinSize         = scalex  32 1.0,
	coinDistanceProp = (1.1,0),
	dialCoinProp     = (0.25,0.05),
	governmentProp   = (0.02,0.43),
	oneCultureSize   = scalex 32 1.0,
	oneCultureDistProp   = (1.1,0),
	fiveCultureSize  = scalex 45 1.0,
	fiveCultureDistProp  = (1.1,0),
	cultureRowsProp      = (0.25,0.72),
	cultureRowsDistProp  = (0,0.15)
	}
	where
	scalex :: Double -> Double -> Size
	scalex x yfactor = (x,x*yfactor)


displayInfoFactory :: Double -> Game -> DisplayInfo
displayInfoFactory scale game = defaultDisplayInfo {
	scaleCoor = \ coor -> show $ round $ coor * scale,
	boardSize = (tilesmaxx,tilesmaxy) }
	where
	tilesmaxx = (fromIntegral $ Prelude.maximum (map boardTileXcoor $ gameBoardTiles game) + 4) * (fst (tileSize defaultDisplayInfo) / 4)
	tilesmaxy = (fromIntegral $ Prelude.maximum (map boardTileYcoor $ gameBoardTiles game) + 4) * (snd (tileSize defaultDisplayInfo) / 4)

tile2StaticR :: BoardTile -> Route App
tile2StaticR (BoardTile tileid _ _ discovered _) = StaticR $
	case (((=~)::String -> String -> Bool) (show tileid) "Tile[0-9]+",discovered) of
		(False,False) -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_back.jpg" ] []
		(False,True)  -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_front.jpg" ] []
		(True,False)  -> StaticRoute [ "Images","Tiles","Back.jpg" ] []
		(True,True)   -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ ".jpg" ] []

staticRoute :: (Show a) => String -> a -> Route App
staticRoute folder a = StaticR $ StaticRoute ["Images",toPathPiece folder,toPathPiece (show a) ++ ".jpg"] []

scaleXCoor di sizef = (scaleCoor di) $ fst (sizef di)
scaleYCoor di sizef = (scaleCoor di) $ snd (sizef di)

board di game = [hamlet|
<div .Board .Canvas .NoSpacing>
  $forall boardtile <- gameBoardTiles game
    <img src=@{tile2StaticR boardtile} .#{tile2class boardtile} style=#{tile2style boardtile}>
|]
	where
	tile2class :: BoardTile -> String
	tile2class boardtile = "Tile " ++ show (boardTileOrientation boardtile)

	tile2style :: BoardTile -> String
	tile2style boardtile = printf "position:absolute; left:%spx; top:%spx;"
		((scaleCoor di) (fromIntegral (boardTileXcoor boardtile) * (fst (tileSize di) / 4)))
		((scaleCoor di) (fromIntegral (boardTileYcoor boardtile) * (snd (tileSize di) / 4)))

dial di game player = [hamlet|
<div .Dial .Canvas .NoSpacing>
  <img .Dial src=@{staticRoute "Dials" (playerCiv player)}>
  <img .TradeDial src=@{StaticR $ StaticRoute [ "Images","Dials","Tradedial.gif" ] []} style=#{tradedial2style di game player}>
  <img .CoinDial src=@{StaticR $ StaticRoute [ "Images","Dials","Coindial.gif" ] []} style=#{coindial2style di game player}>
  ^{coins di (dialSize di) (dialCoinProp di) (playerFreeCoins player)}
  <img .VertCard src=@{staticRoute "Government" (playerGovernment player)} style=#{positionProp2style di (dialSize di) (governmentProp di)}>
  ^{cultureTokens di (dialSize di) (cultureRowsProp di) (playerFreeCulture player)}
|]

tradedialDeg game player =
	div (360*(playerDialTrade player - 1)) 28

positionProp2style :: DisplayInfo -> Size -> Proportion -> String
positionProp2style di (w,h) (px,py) = printf "position:absolute; left:%spx; top:%spx; "
	((scaleCoor di) (w*px))
	((scaleCoor di) (h*py))

positionDial :: Pos -> Size -> Proportion -> Size -> Pos
positionDial (offsetx,offsety) (sourcew,sourceh) (propx,propy) (targetw,targeth) =
	(offsetx + targetw*propx - sourcew/2,
	offsety + targeth*propy - sourceh/2)

tradedial2style :: DisplayInfo -> Game -> Player -> String
tradedial2style di game player = printf "position:absolute; left:%spx; top:%spx; transform:rotate(%ideg)"
	((scaleCoor di) x)
	((scaleCoor di) y)
	(tradedialDeg game player)
	where
	(x,y) = positionDial (0,0) (tradeDialSize di) (dialNaveProp di) (dialSize di)

coindial2style :: DisplayInfo -> Game -> Player -> String
coindial2style di game player = printf "position:absolute; left:%spx; top:%spx; transform:rotate(%ideg)"
	((scaleCoor di) x)
	((scaleCoor di) y)
	(tradedialDeg game player + div (360*(coins-1)) 16)
	where
	(x,y) = positionDial (0,0) (coinDialSize di) (dialNaveProp di) (dialSize di)
	coins = playerFreeCoins player -- TODO: Zusätzliche Coins berechnen

itemRow di (targetw,targeth) (propx,propy) staticr itemclass distanceprop itemsize num = [hamlet|
  $forall i <- is
    <img .#{itemclass} style=#{itemstyle i} src=@{staticr}>
|]
	where
	is = [0..(num-1)] :: [Int]
	itemstyle :: Int -> String
	itemstyle i = printf "position:absolute; left:%spx; top:%spx;"
		(scaleCoor di $ targetw*propx + fst distanceprop * fst itemsize * (fromIntegral i))
		(scaleCoor di $ targeth*propy + snd distanceprop * snd itemsize * (fromIntegral i))

coins di targetsize targetprop num = itemRow di targetsize targetprop
	(StaticR $ StaticRoute ["Images","Dials","Coin.gif"] [])
	("Coin"::String) (coinDistanceProp di) (coinSize di) num

cultureTokens di targetsize targetprop5 num = [hamlet|
  ^{culture5Tokens di targetsize targetprop5 (div num 5) }
  ^{culture1Tokens di targetsize targetprop1 (mod num 5) }
|]
	where
	targetprop1 :: Proportion
	targetprop1 = (
		fst targetprop5 + fst (cultureRowsDistProp di),
		snd targetprop5 + snd (cultureRowsDistProp di) )

culture5Tokens di targetsize targetprop num = itemRow di targetsize targetprop
	(StaticR $ StaticRoute ["Images","Dials","5Culture.gif"] [])
	("Culture5"::String) (fiveCultureDistProp di) (fiveCultureSize di) num

culture1Tokens di targetsize targetprop num = itemRow di targetsize targetprop
	(StaticR $ StaticRoute ["Images","Dials","1Culture.gif"] [])
	("Culture1"::String) (oneCultureDistProp di) (oneCultureSize di) num

techCard di techcard = [hamlet|
  <div .Canvas .NoSpacing>
    <img .HorCard src=@{staticRoute "Tech" (techCardTech techcard)}>
    ^{coins di (horCardSize di) (techCardCoinProp di) (techCardCoins techcard)}
|]

techTree di game player = [hamlet|
  <table border=0>
    $forall (i,techcards) <- techss
      <tr>
        $forall j <- Prelude.replicate i 0
          <td>
            <br>
        $forall techcard <- techcards
          <td colspan=2>
            ^{techCard di techcard}
|]
	where
	techss :: [(Int,[TechCard])]
	techss = Prelude.map (\ (i,level) -> (i,filter ((==level).techCardTreeLevel) (playerTechTree player)))
		[(4,TechLevelV),(3,TechLevelIV),(2,TechLevelIII),(1,TechLevelII),(0,TechLevelI)]

--TODO: DisplayInfo in Monade
vertCardRow :: (Show b) => DisplayInfo -> String -> [a] -> (a -> b) -> (a -> Bool) -> (a -> Route App) -> HtmlUrl (Route App)
vertCardRow di folder cards toshowable revealedf backsidestaticrf = [hamlet|
  <table>
    <tr>
      $forall card <- cards
        <td>
          $if revealedf card
            <img .VertCard src=@{staticRoute folder (toshowable card)}>
          $else
            <img .VertCard src=@{backsidestaticrf card}>
|]

playerArea di game playerindex rotation = [hamlet|
<div .NoSpacing style="transform: rotate(#{show rotation}deg)">
  <table .NoSpacing>
    <tr>
      <td valign=bottom>
        ^{techTree di game player}
      <td>
        <table>
          <tr align=right>
            <td>
              ^{vertCardRow di "Policy" (playerPolicies player) id alwaysTrue undefined}
          <tr>
            <td>
              ^{dial di game player}
      <td>
        <table>
          <tr valign=top>
            <td>
              ^{vertCardRow di "GreatPerson" (take 4 (playerGreatPersons player)) greatPersonCardPerson greatPersonCardRevealed greatpersonback}
          <tr>
            <td>
              ^{vertCardRow di "GreatPerson" (drop 4 (playerGreatPersons player)) greatPersonCardPerson greatPersonCardRevealed greatpersonback}
          <tr>
            <td>
              ^{vertCardRow di "CultureCards" (take 4 (playerCultureCards player)) cultureCardEvent cultureCardRevealed culturecardback}
          <tr>
            <td>
              ^{vertCardRow di "CultureCards" (drop 4 (playerCultureCards player)) cultureCardEvent cultureCardRevealed culturecardback}
|]
	where
	alwaysTrue :: Policy -> Bool
	alwaysTrue _ = True
	player :: Player
	player = gamePlayerSequence game !! playerindex
	culturecardback :: CultureCard -> Route App
	culturecardback culturecard = StaticR $ case map ((cultureCardEvent culturecard) `elem`) (map cultureEventsLevel [1,2,3]) of
		[True,False,False] -> StaticRoute [ "Images","CultureCards","CultureLevel1_back.jpg" ] []
		[False,True,False] -> StaticRoute [ "Images","CultureCards","CultureLevel2_back.jpg" ] []
		[False,False,True] -> StaticRoute [ "Images","CultureCards","CultureLevel3_back.jpg" ] []
		l -> error $ "cultureEventsLevels not disjunct: " ++ show l
	greatpersonback :: GreatPersonCard -> Route App
	greatpersonback _ = StaticR _Images_GreatPerson_Back_gif
	-- TODO: Routen statisch machen