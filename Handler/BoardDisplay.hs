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
import Handler.Logic

import Text.Printf
--import Text.Regex.Base
import Text.Regex.TDFA
import Data.Typeable
import Data.List((!!))
import Data.Char
import Data.Maybe

type Coor = Double
type Size = (Coor,Coor)
type Pos = (Coor,Coor)
type Proportion = (Double,Double)

data DisplayInfo = DisplayInfo {
	whoAmI :: PlayerIndex,
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
	cultureRowsDistProp :: Proportion,
	unitCardSize :: Size,
	unitCardDistProp :: Proportion,
	tokenSize :: Size,
	personRowLength :: Int,
	startPlayerSize :: Size,
	playerOrientation :: [Orientation],
	buildingSize :: Size,
	metropolisSize :: Size,
	rotateOrigin :: Pos
	}

defaultDisplayInfo = DisplayInfo {
	whoAmI = undefined,
	scaleCoor        = undefined,
	squareSize       = scalex  93 1.00,
	tileSize         = scalex 372 1.00,
	vertCardSize     = scalex 122 1.54,
	horCardSize      = scalex 187 0.65,
	dialSize         = scalex 561 0.65,
	dialNaveProp     = (0.754,0.355),
	tradeDialSize    = scalex 168 1.40,
	coinDialSize     = scalex  83 1.73,
	boardSize        = undefined,
	techCardCoinProp = (0.24,0.40),
	coinSize         = scalex  32 1.0,
	coinDistanceProp = (1.1,0),
	dialCoinProp     = (0.25,0.05),
	governmentProp   = (0.02,0.43),
	oneCultureSize   = scalex 32 1.0,
	oneCultureDistProp   = (1.1,0),
	fiveCultureSize      = scalex 45 1.0,
	fiveCultureDistProp  = (1.1,0),
	cultureRowsProp      = (0.25,0.72),
	cultureRowsDistProp  = (0,0.15),
	unitCardSize     = scalex 202 1.0,
	unitCardDistProp = (0,0.38),
	tokenSize        = scalex 60 1.0,
	personRowLength  = 4,
	startPlayerSize  = scalex 187 1.0,
	playerOrientation = undefined,
	buildingSize     = scalex 83 1.0,
	metropolisSize   = undefined,
	rotateOrigin     = undefined
	}
	where
	scalex :: Double -> Double -> Size
	scalex x yfactor = (x,x*yfactor)


displayInfoFactory :: PlayerIndex -> Double -> Game -> [Orientation] -> DisplayInfo
displayInfoFactory whoami scale game orientations = defaultDisplayInfo {
	whoAmI = whoami,
	scaleCoor = \ coor -> show $ round $ coor * scale,
	boardSize = (tilesmaxx,tilesmaxy),
	playerOrientation = orientations,
	metropolisSize =
		(fst (buildingSize defaultDisplayInfo) + fst (squareSize defaultDisplayInfo),
		snd (buildingSize defaultDisplayInfo)),
	rotateOrigin = (fst (buildingSize defaultDisplayInfo) / 2,snd (buildingSize defaultDisplayInfo) / 2)

	}
	where
	tilesmaxx = (fromIntegral $ Prelude.maximum (map boardTileXcoor $ gameBoardTiles game) + 4) *
		(fst (tileSize defaultDisplayInfo) / 4)
	tilesmaxy = (fromIntegral $ Prelude.maximum (map boardTileYcoor $ gameBoardTiles game) + 4) *
		(snd (tileSize defaultDisplayInfo) / 4)

tile2StaticR :: BoardTile -> Route App
tile2StaticR (BoardTile tileid _ _ discovered _) = StaticR $
	case (((=~)::String -> String -> Bool) (show tileid) "Tile[0-9]+",discovered) of
		(False,False) -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_back.jpg" ] []
		(False,True)  -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ "_front.jpg" ] []
		(True,False)  -> StaticRoute [ "Images","Tiles","Back.jpg" ] []
		(True,True)   -> StaticRoute [ "Images","Tiles",toPathPiece (show tileid) ++ ".jpg" ] []

staticRoute :: (Show a) => (String,String) -> a -> Route App
staticRoute (folder,extension) a = StaticR $ StaticRoute [
	"Images",
	toPathPiece folder,
	toPathPiece (show a) ++ "." ++ toPathPiece extension] []

scaleXCoor di sizef = (scaleCoor di) $ fst (sizef di)
scaleYCoor di sizef = (scaleCoor di) $ snd (sizef di)

centreCoor (x0,y0) (w0,h0) (itemw,itemh) = ( x0+(w0-itemw)/2, y0+(h0-itemh)/2 )

divCoor (x,y) f = (x/f,y/f)

colourString :: Colour -> String
colourString colour = map Data.Char.toLower (show colour)

--TODO: Mit Table/rowspan/colspan einfacher?
board di game = [hamlet|
<div .Board .Canvas .NoSpacing>
  $forall boardtile <- gameBoardTiles game
    <img src=@{tile2StaticR boardtile} .#{tile2class boardtile} style=#{to_style (squarecoor (boardTileXcoor boardtile) (boardTileYcoor boardtile))}>
  $forall (playerindex,player) <- players
    $forall city <- playerCities player
      <img .#{city_to_class city playerindex} style=#{to_style (buildingcoor (cityXCoor city) (cityYCoor city))} src=@{tocitysrc player city}>
|]
	where
	players = zip [0..] (gamePlayerSequence game)

	tocitysrc :: Player -> City -> Route App
	tocitysrc player city = StaticR $ StaticRoute ["Images","Squares",toPathPiece name] [] where
		name = printf "%s%i_%s.jpg"
			(case cityType city of
				PlainCity -> "City" :: String
				Metropolis -> "Metropolis" :: String)
			(basicCityDefence city)
			(colourString $ playerColour player) :: String

	tile2class :: BoardTile -> String
	tile2class boardtile = "Tile " ++ show (boardTileOrientation boardtile)

	buildingcoor x y = centreCoor (squarecoor x y) (divCoor (tileSize di) 4) (buildingSize di)

	squarecoor x y = (
		fromIntegral x * (fst (tileSize di) / 4),
		fromIntegral y * (snd (tileSize di) / 4) )

	to_style :: Pos -> String
	to_style (x,y) = printf "position:absolute; left:%spx; top:%spx"
		((scaleCoor di) x) ((scaleCoor di) y)

	city_to_class city playerindex =
		case cityType city of
			PlainCity -> "Building" :: String
			Metropolis -> "Metropolis" :: String
		++
		(" " :: String) ++
		show (addOrientation (playerOrientation di !! playerindex) (cityOrientation city))

dial di game player = [hamlet|
<div .Dial .Canvas .NoSpacing>
  <img .Dial src=@{staticRoute ("Dials","jpg") (playerCiv player)}>
  <img .TradeDial src=@{StaticR _Images_Dials_Tradedial_gif} style=#{tradedial2style di game player}>
  <img .CoinDial src=@{StaticR _Images_Dials_Coindial_gif} style=#{coindial2style di game player}>
  ^{coins di (dialSize di) (dialCoinProp di) (playerFreeCoins player)}
  <img .VertCard src=@{staticRoute ("Government","jpg") (playerGovernment player)} style=#{positionProp2style di (dialSize di) (governmentProp di)}>
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

itemRow di (targetw,targeth) (propx,propy) staticr_rots itemclass distanceprop itemsize = [hamlet|
  $forall (i,(staticr,rot)) <- zip indices staticr_rots
    <img .#{itemclass} style=#{itemstyle i rot} src=@{staticr}>
|]
	where
	indices = [0..] :: [Int]
	itemstyle :: Int -> Int -> String
	itemstyle i rot = printf "position:absolute; left:%spx; top:%spx; transform: rotate(%ideg);"
		(scaleCoor di $ targetw*propx + fst distanceprop * fst itemsize * (fromIntegral i))
		(scaleCoor di $ targeth*propy + snd distanceprop * snd itemsize * (fromIntegral i))
		rot

coins di targetsize targetprop num = itemRow di targetsize targetprop
	(Prelude.replicate num (StaticR _Images_Dials_Coin_gif,0))
	("Coin"::String) (coinDistanceProp di) (coinSize di)

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
	(Prelude.replicate num (StaticR _Images_Dials_5Culture_gif,0))
	("Culture5"::String) (fiveCultureDistProp di) (fiveCultureSize di)

culture1Tokens di targetsize targetprop num = itemRow di targetsize targetprop
	(Prelude.replicate num (StaticR _Images_Dials_1Culture_gif,0))
	("Culture1"::String) (oneCultureDistProp di) (oneCultureSize di)

techCard di techcard = [hamlet|
  <div .Canvas .NoSpacing>
    <img .HorCard src=@{staticRoute ("Tech","jpg") (techCardTech techcard)}>
    ^{coins di (horCardSize di) (techCardCoinProp di) (techCardCoins techcard)}
|]

techTree di game playerindex = [hamlet|
<div .Canvas .NoSpacing>
  <table border=0>
    <colgroup>
      $forall j <- columns
        <col width=#{(scaleCoor di) colwidth}>
    $forall (i,techcards) <- techss
      <tr>
        $forall j <- Prelude.replicate i 0
          <td>
            <br>
        $forall techcard <- techcards
          <td colspan=2>
            ^{techCard di techcard}
  $if startplayer
    <img .StartPlayer style="position:absolute; right:5px; top:0px;" src=@{StaticR _Images_StartPlayer_gif}>
|]
	where
	startplayer = gameStartPlayer game == playerindex
	player = gamePlayerSequence game !! playerindex
	columns :: [Int]
	columns = [1 .. (length $ fromJust $ lookup 0 techss)]
	colwidth :: Coor
	colwidth = fst (horCardSize di) / 2
	techss :: [(Int,[TechCard])]
	techss = Prelude.map (\ (i,level) -> (i,filter ((==level).techCardTreeLevel) (playerTechTree player)))
		[(4,TechLevelV),(3,TechLevelIV),(2,TechLevelIII),(1,TechLevelII),(0,TechLevelI)]

--TODO: DisplayInfo in Monade
vertCardRow :: (Show b) => DisplayInfo -> PlayerIndex -> String -> [a] -> (a -> b) -> (a -> Bool) -> (a -> Route App) -> HtmlUrl (Route App)
vertCardRow di playerindex folder cards toshowable revealedf backsidestaticrf = [hamlet|
  <table>
    <tr>
      $forall card <- cards
        <td>
          $if reveal card
            <img .VertCard src=@{staticRoute folderext (toshowable card)}>
          $else
            <img .VertCard src=@{backsidestaticrf card}>
|]
	where
	folderext = (folder,"jpg")
	reveal card = revealedf card || (whoAmI di == playerindex)

unitColumn di game playerindex = [hamlet|
  <div .Canvas .NoSpacing>
    ^{itemRow di zero zero staticr_rots itemclass distanceprop itemsize}
|]
	where
	player = gamePlayerSequence game !! playerindex
	units = playerUnits player
	zero = (0.0,0.0)
	staticr_rots = case playerindex == whoAmI di of
		True -> zip (map (staticRoute ("Unit","jpg")) units) (map rot units)
		False -> map (\ _ -> (StaticR _Images_Unit_Unit_back_jpg,0)) units
	rot unit = (unitLevel unit player - 1) * (-90)
	itemclass = "Unit"::String
	distanceprop = unitCardDistProp di
	itemsize = unitCardSize di

itemTokens di game playerindex = [hamlet|
  $forall staticr <- tokens
    <img .Token style="float:left" src=@{staticr}>
|]
	where
	reveal = playerindex == whoAmI di
	tostaticr :: (Show a) => Route App -> a -> Route App
	tostaticr conceilstaticr token = case reveal of
		True -> staticRoute ("Resource","gif") token
		False -> conceilstaticr
	player = gamePlayerSequence game !! playerindex
	resources = map (staticRoute ("Resource","gif")) $
		sort (playerResources player)
	huts = map (tostaticr (StaticR _Images_Resource_Hut_gif)) $
		sort (playerHuts player)
	villages = map (tostaticr (StaticR _Images_Resource_Village_gif)) $
		sort (playerVillages player)
	tokens = resources ++ villages ++ huts

playerArea di game playerindex = [hamlet|
<div .NoSpacing .#{playerclass}>
  <table .NoSpacing>
    <tr>
      <td valign=bottom>
        ^{techTree di game playerindex}
      <td style="max-width:#{(scaleCoor di) (fst (dialSize di))}px">
        <table>
          <tr align=right>
            <td>
              <table>
                <tr>
                  <td align=left>
                    ^{itemTokens di game playerindex}
                  <td align=right>
                    ^{vertCardRow di playerindex "Policy" (playerPolicies player) id alwaysTrue undefined}
          <tr>
            <td>
              ^{dial di game player}
      <td valign=top>
        <table style="clear: both; overflow: visible">
          <tr>
            <td style="valign:top;">
              ^{vertCardRow di playerindex "GreatPerson" (take personswidth $ playerGreatPersons player) greatPersonCardPerson greatPersonCardRevealed greatpersonback}
          <tr>
            <td style="valign:top;">
              ^{vertCardRow di playerindex "GreatPerson" (drop personswidth $ playerGreatPersons player) greatPersonCardPerson greatPersonCardRevealed greatpersonback}
          <tr>
            <td style="valign:top;">
              ^{vertCardRow di playerindex "CultureCards" (take personswidth $ playerCultureCards player) cultureCardEvent cultureCardRevealed culturecardback}
          <tr>
            <td style="valign:top;">
              ^{vertCardRow di playerindex "CultureCards" (drop personswidth $ playerCultureCards player) cultureCardEvent cultureCardRevealed culturecardback}
      <td valign=top>
        ^{unitColumn di game playerindex}
|]
	where 
	playerclass = show $ playerOrientation di !! playerindex 
	personswidth = personRowLength di
	alwaysTrue :: Policy -> Bool
	alwaysTrue _ = True
	player :: Player
	player = gamePlayerSequence game !! playerindex
	culturecardback :: CultureCard -> Route App
	culturecardback culturecard = case map ((cultureCardEvent culturecard) `elem`) (map cultureEventsLevel [1,2,3]) of
		[True,False,False] -> StaticR _Images_CultureCards_CultureLevel1_back_jpg
		[False,True,False] -> StaticR _Images_CultureCards_CultureLevel2_back_jpg
		[False,False,True] -> StaticR _Images_CultureCards_CultureLevel3_back_jpg
		l -> error $ "cultureEventsLevels not disjunct: " ++ show l
	greatpersonback :: GreatPersonCard -> Route App
	greatpersonback _ = StaticR _Images_GreatPerson_Back_gif
