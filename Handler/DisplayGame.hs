{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module Handler.DisplayGame where

import Import hiding (map,minimum,maximum,concat,lookup,replicate,head)
import Prelude (map,minimum,maximum,concat,lookup,replicate,tail,init,head)

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Array.IArray ((!),indices)
import Data.Maybe
import Control.Lens hiding (indices,Action)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 (unpack)
import qualified Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze (string)

import GameMonad
import Model
import Entities
import Polls
import Handler.HandlerPolling
import Handler.StaticResources
import Lenses
import Logic
import TokenStack
import Actions


data2markup :: (ToJSON a) => a -> String
data2markup a = Text.Blaze.Renderer.String.renderHtml $ string $ show $
	Data.ByteString.Lazy.Char8.unpack $ encode a

colour2html :: Colour -> String
colour2html colour = show colour

-- <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade

data DisplayInfo = DisplayInfo {
	gameNameDI :: GameName,
	gameDI :: Game,
	myPlayerNameDI :: Maybe PlayerName,
	myPlayerDI :: Maybe Player,
	myPlayerOriDI :: Orientation,
	playernameToPlayerDI :: (PlayerName -> Player)
}

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	let
		toplayer playername = fromJust $ lookupAssocList playername (_gamePlayers game)
		mb_myplayer = fmap toplayer mb_playername
		myplayerori = maybe Northward _playerOrientation mb_myplayer
		di = DisplayInfo gamename game mb_playername mb_myplayer myplayerori toplayer
		(playernametomove,_) = nthAssocList (_gamePlayersTurn game) (_gamePlayers game)
		moves = moveGen gamename game mb_playername
	playerareas <- mapM (playerArea di) $ fromAssocList (_gamePlayers game)
	playerlist <- playerList di
	boardarea <- boardArea di moves
	actionarea <- actionArea di mb_playername
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)

		let arena = case playerareas of
			[playerarea0,playerarea1] -> [whamlet|
<div class=#{show myplayerori}>
  <table cellspacing=20>
    <tr><td>^{playerarea0}
    <tr><td>^{boardarea}
    <tr><td>^{playerarea1}
|]
			[playerarea0,playerarea1,playerarea2,playerarea3] -> [whamlet|
<table>
  <tr>
    <td colspan="2">^{playerarea0}
    <td rowspan="2">^{playerarea1}
  <tr>
    <td rowspan="2">^{playerarea3}
    <td>^{boardarea}
  <tr>
    <td colspan="2">^{playerarea2}
|]
			pas -> errHamlet $ "Layout for " ++ show (length pas) ++ " not implemented (yet)."

		[whamlet|
<table>
  <tr>
    <td><div style="overflow:auto">^{arena}
    <td valign=bottom>
      <table>
        <tr><td>^{playerlist}
        <tr><td>^{actionarea}
|]

actionArea :: DisplayInfo -> Maybe PlayerName -> Handler Widget
actionArea _ Nothing = return [whamlet|
Visitor
|]
actionArea di@(DisplayInfo{..}) (Just playername) = do
	let actions = map (("Action "++).show) [1..5]
	return [whamlet|
<table>
  $forall action <- actions
    <tr><td>#{action}
|]

playerList :: DisplayInfo -> Handler Widget
playerList di@(DisplayInfo{..}) = do
	let players = fromAssocList $ _gamePlayers gameDI
	let playerlist = map (\ (playername,player@Player{..}) -> (_playerCiv,playername,_playerColour)) players
	return [whamlet|
<table>
  $forall (civ,playername,colour) <- playerlist
    <tr bgcolor="#{colour2html colour}">
      <td>#{show civ}
      <td>#{show playername}
|]

playerArea :: DisplayInfo -> (PlayerName,Player) -> Handler Widget
playerArea di@(DisplayInfo{..}) (playername,player@(Player{..})) = do
	let
		reveal = isNothing myPlayerNameDI || (Just playername == myPlayerNameDI)
	greatpersonsrow <- horRow reveal _greatPersonCardRevealed greatPersonRoute _greatPerson _playerGreatPersonCards
	culturecardsrow <- horRow reveal _cultureCardRevealed cultureRoute _cultureCardEvent _playerCultureCards
	policyrow <- horRow True (const True) (\ pol _ -> policyRoute pol) id (snd _playerPolicies)
	techtree <- techTree di (playername,player)
	items <- itemTokens di player reveal
	unitcolumn <- unitColumn di (playername,player)
	pdial <- dial di player
	cityItems <- partialCityItems di player

	return [whamlet|
<div .PlayerArea style="border: 10px solid #{show _playerColour};">
  ^{cityItems}
<div .NoSpacing class="Debug-Draggable PlayerArea2 #{show _playerOrientation}" style="border: 10px solid #{show _playerColour};">
  <table .NoSpacing>
    <tr>
      <td>
        <table .NoSpacing>
          <tr>
            <td colspan=2 align=center>
              <table>
                <tr>
                  <td style="valign:top; align:right">
                    ^{greatpersonsrow}
                  <td style="valign:top; align:left">
                    ^{culturecardsrow}
          <tr>
            <td valign=bottom .Parent .NoSpacing>
              ^{techtree}
            <td>
              <table>
                <tr>
                  <td>
                    <table style="max-width:100%">
                      <tr>
                        <td align=left>
                          ^{items}
                        <td align=right>
                          ^{policyrow}
                <tr>
                  <td>
                    ^{pdial}
      <td valign=top>
        ^{unitcolumn}
|]

horRow :: Bool -> (card -> Bool) -> (val -> Bool -> Route App) -> (card -> val) -> [card] -> Handler Widget
horRow reveal revealcard card2route card2val cards = return [whamlet|
<div>
  <table>
    <tr>
      $forall card <- cards
        <td><img src=@{cardroute card}>
|]
	where
	cardroute card = card2route (card2val card) (reveal || revealcard card)

itemTokens :: DisplayInfo -> Player -> Bool -> Handler Widget
itemTokens di player reveal = return [whamlet|
<div>
  $forall route <- routes
    <img style="float:left" src=@{route}>
|]
	where
	(resources,huts,villages,artifacts) = _playerItems player
	routes =
		map resourceRoute resources ++
		map (if reveal then revealedHutRoute else const hutRoute) huts ++
		map (if reveal then revealedVillageRoute else const villageRoute) villages ++
		map artifactRoute artifacts

dial :: DisplayInfo -> Player -> Handler Widget
dial di player@(Player{..}) = do
	let
		tradeangle = div ((tradeTrade _playerTrade - 1) * 360) 28
		coinangle = tradeangle + div ((coinsCoins _playerCoins - 1) * 360) 16
		Culture culture = _playerCulture
		culturerow =
			replicate (div culture 5) fiveCultureRoute ++
			replicate (mod culture 5) oneCultureRoute
		coinrow = replicate (coinsCoins _playerCoins) coinRoute
	return [whamlet|
<div style="width:570px; height:380px;" alt="alt" title="#{show player}">
  <div .Center .Parent>
    <img .Child style="left:0px; top:0px" src=@{dialRoute _playerCiv}>
    <img .Child style="left:343px; top:19px; #{rotateStyle tradeangle}" src=@{tradeDialRoute}>
    <img .Child style="left:385px; top:65px; #{rotateStyle coinangle}" src=@{coinDialRoute}>
    <img .Child style="left:15px; top:165px" src=@{governmentRoute _playerGovernment}>
    <div .Child style="left:180px; top:270px">
      <table .Parent .NoSpacing>
        <tr>
          $forall route <- coinrow
            <td><img .Parent style="float:left" src=@{route}>
    <div .Child style="left:180px; top:315px">
      <table .Parent .NoSpacing>
        <tr>
          $forall route <- culturerow
            <td><img .Parent style="float:left;" src=@{route}>
|]

unitColumn :: DisplayInfo -> (PlayerName,Player) -> Handler Widget
unitColumn di@(DisplayInfo{..}) (playername,player@(Player{..})) = do
	let
		reveal = isNothing myPlayerNameDI || (Just playername == myPlayerNameDI)
		unitlevel = playerUnitLevel player
		unitcards = map (\ uc@(UnitCard{..}) -> (uc,unit2Ori (fromJust $ unitlevel unitType))) $ sort _playerUnits
	return [whamlet|
<table>
  $forall (unitcard,ori) <- unitcards
    <tr>
      <td>
        <div style="height:44px; width:205px; overflow:visible" >
          <img class="#{show ori}" style="transform-origin: 50% 50%" src=@{unitCardRoute unitcard reveal}>
|]

coinRow :: Coins -> Widget
coinRow coins = [whamlet|
<table .NoSpacing>
  <tr>
    $forall route <- coinroutes
      <td><img src=@{route}>
|]
	where
	coinroutes = replicate (coinsCoins coins) coinRoute

techTree :: DisplayInfo -> (PlayerName,Player) -> Handler Widget
techTree di@(DisplayInfo{..}) (playername,player@(Player{..})) = do
	let
		game@(Game{..}) = gameDI
		startplayer = playername == fst (nthAssocList _gameStartPlayer _gamePlayers)
		Just unusedflags = tokenStackLookup Flag _playerFigures
		Just unusedwagons = tokenStackLookup Wagon _playerFigures
		Just leftcities = tokenStackLookup () _playerCityStack
		techss :: [(Int,[TechCard])]
		techss = map projecttechlevel
			[(4,TechLevelV),(3,TechLevelIV),(2,TechLevelIII),(1,TechLevelII),(0,TechLevelI)]
		projecttechlevel (i,level) = ( i, filter ((==level)._techCardLevel) _playerTechs )
		columns = fromJust $ lookup 0 techss
		canbuildmetropolis = getValueAbility canBuildMetropolis player
	Just mb_capitalmetropolis <- case _playerCityCoors of
		(capitalcoors:_) -> queryCivLensH $ civSquareLens gameNameDI capitalcoors . squareTokenMarker . _Just . cityMarker . cityMetropolisOrientation
		_ -> return $ Just Nothing
	let showmetropolis = isNothing mb_capitalmetropolis && getValueAbility canBuildMetropolis player
	return [whamlet|
<div>
  <div .Parent .NoSpacing>
    <table border=0>
      <colgroup>
        $forall j <- columns
          <col width=94px>
      $forall (i,techcards) <- techss
        <tr>
          $forall j <- replicate i 0
            <td><br>
          $forall techcard@TechCard{..} <- techcards
            <td colspan=2>
              <div>
                <img src=@{techRoute _techCardTechId}>
                <div .TechCoinPos>
                  ^{coinRow _techCardCoins}
  $if startplayer
    <img style="position:absolute; left:5px; top:0px;" src=@{startPlayerRoute}>
  <div style="position:absolute; right:5px; top:0px;">
    <table .NoSpacing>
      <tr>
        <td valign=top>
          <table .NoSpacing>
            $forall i <- unusedwagons
              <tr><td><img .Wagon data-source=#{data2markup WagonSource} src=@{wagonRoute _playerColour}>
        <td valign=top>
          <table .NoSpacing>
            $forall i <- unusedflags
              <tr><td><img .Flag data-source=#{data2markup FlagSource} src=@{flagRoute _playerColour}>
        <td valign=top>
          $forall _ <- leftcities 
            <img data-source=#{data2markup CitySource} src=@{cityRoute' (False,NoWalls,_playerColour)}>
        <td valign=top>
          $if showmetropolis
            <img data-source=#{data2markup MetropolisSource} src=@{metropolisRoute' (NoWalls,_playerColour)}>
|]

boardArea :: DisplayInfo -> [Action] -> Handler Widget
boardArea (DisplayInfo{..}) actions = do
	let
		arr = _gameBoard gameDI
		arrlookup coors = arr!coors
		allcoors = indices arr
		(xcoors,ycoors) = (map xCoor allcoors,map yCoor allcoors)
		xs = [(minimum xcoors)..(maximum xcoors)]
		ys = [(minimum ycoors)..(maximum ycoors)]
		playerori owner = _playerOrientation (playernameToPlayerDI owner)
		playercolour owner = _playerColour (playernameToPlayerDI owner)
		cityori (city@City{..}) = case _cityMetropolisOrientation of
			Nothing -> pori
			Just metropolisori -> addOri metropolisori Westward
			where
			pori = playerori _cityOwner

		rowcolspan :: Coors -> Maybe (Int,Int,String)
		rowcolspan coors = case arrlookup coors of
			Square _ _ _ _ _ (Just (CityMarker city)) _ _ -> case city of
				SecondCitySquare _          -> Nothing
				City _ _ _ _ _ _ Nothing    -> Just (1,1,"SquareContainer")
				City _ _ _ _ _ _ (Just ori) -> case ori of
					Southward -> Just (2,1,"VertDoubleSquareContainer")
					Eastward  -> Just (1,2,"HorDoubleSquareContainer")
					_         -> error $ "Strange orientation: " ++ show coors
			_ -> Just (1,1,"SquareContainer")

	return [whamlet|
<div .Parent>
  <div .Child style="z-index: 1;">
    <table .NoSpacing border=1>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $with square <- arrlookup (Coors x y)
              $maybe (rowspan,colspan,sizeclass) <- rowcolspan (Coors x y)
                <td .SquareContainer.Map-SquareContainer data-target=#{data2markup $ filter (coors2action (Coors x y)) actions} rowspan="#{show rowspan}" colspan="#{show colspan}" alt="alt" title="#{(++) (show (x,y)) (show square)}" style="position:relative">
                  $case square
                    $of OutOfBounds
                    $of UnrevealedSquare _ _
                    $of _
                      $maybe tokmarker <- _squareTokenMarker square
                        $case tokmarker
                          $of ArtifactMarker artifact
                            <img .Center class="#{show myPlayerOriDI}" src=@{artifactRoute artifact}>
                          $of HutMarker _
                            <img .Center class="#{show myPlayerOriDI}" src=@{hutRoute}>
                          $of VillageMarker _
                            <img .Center class="#{show myPlayerOriDI}" src=@{villageRoute}>
                          $of CityMarker (city@(City{..}))
                             <div class="#{sizeclass}">
                               <div .Center class="#{show (cityori city)}Square">
                                 <img src=@{cityRoute (playercolour _cityOwner) city}>
                          $of CityMarker (SecondCitySquare _)
                          $of BuildingMarker (Building buildingtype owner)
                            <img .Center class="#{show (playerori owner)}Square" src=@{buildingTypeRoute buildingtype}>

  <div style="z-index: 2;">
    <table .NoSpacing>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $case arrlookup (Coors x y)
              $of OutOfBounds
                <td .TileContainer><img .Center src=@{transparentSquareRoute}> 
              $of UnrevealedSquare tileid coors
                $if (==) coors (Coors x y)
                  <td .TileContainer colspan=4 rowspan=4><img .Center class=#{show Northward} src=@{boardTileRoute tileid False}>
              $of sq
                $maybe (tileid,ori) <- _squareTileIDOri sq
                  <td .TileContainer colspan=4 rowspan=4><img .Center class=#{show ori} src=@{boardTileRoute tileid True}>
|]

rotateStyle deg =
	"-moz-transform: rotate(" ++ show deg ++ "deg); " ++
	"-webkit-transform: rotate(" ++ show deg ++ "deg); " ++
    "-o-transform: rotate(" ++ show deg ++ "deg); " ++
    "transform: rotate(" ++ show deg ++ "deg); " ++
	"transform-origin: 50% 50%; "

partialCityItems :: DisplayInfo -> Player -> Handler Widget
partialCityItems di player@(Player{..}) = do
    return [whamlet|
<div class="PlayerArea-CityItems">
    <div class="PlayerArea-CityItem" data-source=#{data2markup CitySource}><img src=@{cityRoute' (False,NoWalls,_playerColour)}>
    <div class="PlayerArea-CityItem" data-source=#{data2markup MetropolisSource}><img src=@{metropolisRoute' (NoWalls,_playerColour)}>
|]
	
