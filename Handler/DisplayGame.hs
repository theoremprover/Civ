{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

module Handler.DisplayGame where

import Import hiding (map,minimum,maximum,concat,lookup,replicate,head,zip)
import Prelude (map,minimum,maximum,concat,lookup,replicate,tail,init,head,zip)

import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.Array.IArray ((!),indices)
import Data.Maybe
import Control.Lens hiding (indices,Action)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 (unpack)
import qualified Text.Blaze.Renderer.String (renderHtml)
import Text.Blaze (string)
import Data.Text.Lazy.Builder (fromString)
import Text.Julius (RawJavascript(..))
import Data.Acid.Advanced
import Data.List ((!!))

import GameMonad
import Model
import Entities
import Polls
import Handler.HandlerPolling
import Handler.StaticResources
import Lenses
import Logic
import TokenStack
import Acidic

default (Int, Float)


movelisttarget2markup :: [Move] -> String
movelisttarget2markup []    = data2markup $ NoTarget ()
movelisttarget2markup (Move _ target : _) = data2markup target

data2markup :: (ToJSON a) => a -> String
data2markup a = Data.ByteString.Lazy.Char8.unpack $ encode a

colour2html :: Colour -> String
colour2html colour = show colour

-- <button type=button onclick=#{onclickHandler $ IncTradeA gamename playername (Trade 1)}>IncTrade

data DisplayInfo = DisplayInfo {
	gameNameDI           :: GameName,
	gameDI               :: Game,
	myPlayerNameDI       :: Maybe PlayerName,
	myPlayerDI           :: Maybe Player,
	myPlayerOriDI        :: Orientation,
	playernameToPlayerDI :: (PlayerName -> Player)
}

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	app <- getYesod
	moves <- case mb_playername of
		Nothing -> return []
		Just playername -> update' (appCivAcid app) $ MoveGen gamename playername 
	let
		toplayer playername = fromJust $ lookupAssocList playername (_gamePlayers game)
		mb_myplayer = fmap toplayer mb_playername
		myplayerori = maybe Northward _playerOrientation mb_myplayer
		di = DisplayInfo gamename game mb_playername mb_myplayer myplayerori toplayer
		(playernametomove,_) = nthAssocList (_gamePlayersTurn game) (_gamePlayers game)
	playerlist <- playerList di
	boardarea <- boardArea di moves
	actionarea <- actionArea di mb_playername moves
	debugarea <- partialDebugArea di
	arena <- case fromAssocList (_gamePlayers game) of
		[p0,p1] -> do
			playerarea0 <- playerArea di p0 Southward
			playerarea1 <- playerArea di p1 Northward
			return [whamlet|
<div class=#{show myplayerori}>
  <table cellspacing=20>
    <tr><td>^{playerarea0}
    <tr><td>^{boardarea}
    <tr><td>^{playerarea1}
|]

		[p0,p1,p2] -> do
			playerarea0 <- playerArea di p0 Southward
			playerarea1 <- playerArea di p1 Westward
			playerarea2 <- playerArea di p2 Eastward
			return [whamlet|
<table>
  <tr>
    <td>
    <td>^{playerarea0}
    <td>
  <tr>
    <td>^{playerarea1}
    <td>^{boardarea}
    <td>^{playerarea2}
|]

		[p0,p1,p2,p3] -> do
			playerarea0 <- playerArea di p0 Southward
			playerarea1 <- playerArea di p1 Westward
			playerarea2 <- playerArea di p2 Northward
			playerarea3 <- playerArea di p3 Eastward
			return [whamlet|
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

		[p0,p1,p2,p3,p4] -> do
			playerarea0 <- playerArea di p0 Southward
			playerarea1 <- playerArea di p1 Westward
			playerarea2 <- playerArea di p2 Westward
			playerarea3 <- playerArea di p3 Northward
			playerarea4 <- playerArea di p4 Eastward
			return [whamlet|
<table>
  <tr>
    <td>
    <td>^{playerarea0}
    <td rowspan="2">^{playerarea1}
  <tr>
    <td rowspan="2">^{playerarea4}
    <td .Centre rowspan="2">^{boardarea}
  <tr>
    <td rowspan="2">^{playerarea2}
  <tr>
    <td>
    <td colspan="2">^{playerarea3}
|]

		[p0,p1,p2,p3,p4,p5] -> do
			playerarea0 <- playerArea di p0 Southward
			playerarea1 <- playerArea di p1 Westward
			playerarea2 <- playerArea di p2 Westward
			playerarea3 <- playerArea di p3 Northward
			playerarea4 <- playerArea di p4 Eastward
			playerarea5 <- playerArea di p5 Eastward
			return [whamlet|
<table>
  <tr>
    <td rowspan="3">^{playerarea5}
    <td>^{playerarea0}
    <td rowspan="2">^{playerarea1}
  <tr>
    <td rowspan="3">^{boardarea}
  <tr>
    <td rowspan="2">^{playerarea2}
  <tr>
    <td rowspan="2">^{playerarea4}
  <tr>
    <td>^{playerarea3}
|]

		pas -> error $ "Layout for " ++ show (length pas) ++ " players not implemented (yet)."
	Just dbgmsg <- queryCivLensH civDebugMsg
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		sendJSONJulius
		longPollingJulius (GameR $ gameName gamename) (GameGame gamename)
		allowedMovesJulius moves

		[whamlet|
<table>
  <tr>
    <td><div style="overflow:auto">^{arena}
    <td valign=bottom>
      <table>
        <tr><td>^{playerlist}
        <tr><td>^{actionarea}

<div .Sidebar>
  <table>
    <tr><td>^{playerlist}
    <tr><td>^{actionarea}
    <tr><td><a href="#" class="Action-OpenDebug">Show Debug
    <tr><td><a href="#" class="Action-ShowDialog" onclick="showDialog()">Show Dialog
    <tr><td><a href=@{HomeR}>Change

<div id="Dialog-Test" title="TestDialog">
  <p>
    <span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;">
    Click <a href=@{HomeR}>here</a> to switch to the next player
  <p>

<div .DragArea>
<div .DebugArea>
  Debug<br />
  <p border=1 bgcolor=yellow>#{dbgmsg}</ br>
  ^{debugarea}
  <a href="#" class="Action-CloseDebug">close</a>
|]

allowedMovesJulius :: [Move] -> Widget
allowedMovesJulius moves = toWidget [julius|
var allowedMoves = #{RawJavascript $ Data.Text.Lazy.Builder.fromString $ data2markup moves};
|]

actionArea :: DisplayInfo -> Maybe PlayerName -> [Move] -> Handler Widget
actionArea _ Nothing _ = return [whamlet|
Visitor
|]
actionArea di@(DisplayInfo{..}) (Just playername) moves = do
	return [whamlet|
<div style="overflow:scroll">
  <table .ActionArea>
    $forall move <- moves
      <tr><td><button type=button onclick="sendAction(JSON.stringify(#{data2markup $ GameActionA move}))">#{show move}
|]

playerList :: DisplayInfo -> Handler Widget
playerList di@(DisplayInfo{..}) = do
	let
		Game{..} = gameDI
		playerlist = map (\ (i,(playername,player@Player{..})) -> (
			Just playername == myPlayerNameDI,
			i==_gameStartPlayer,
			i==_gamePlayersTurn,
			_playerCiv,playerName playername,_playerColour))
			(zip [0..] $ fromAssocList _gamePlayers)
	return [whamlet|
<table .PlayerList>
  <tr>
    <td>
    <td>
    <td colspan=2>Turn #{show _gameTurn}
  <tr>
    <td>
    <td>
    <td colspan=2>#{show _gamePhase}
  $forall (itsme,startplayer,playersturn,civ,playername,colour) <- playerlist
    <tr>
      <td>
        $if startplayer
          <img style="height:1.5em; width:auto;" src=@{startPlayerRoute}>
      <td>
        $if playersturn
          &#8594;
      <td bgcolor="#{colour2html colour}">#{show civ}
      $if itsme
        <td bgcolor="lightgray">#{playername}
      $else
        <td>#{playername}
|]

playerArea :: DisplayInfo -> (PlayerName,Player) -> Orientation -> Handler Widget
playerArea di@(DisplayInfo{..}) (playername,player@(Player{..})) playerarea_ori = do
	let
		reveal = isNothing myPlayerNameDI || (Just playername == myPlayerNameDI)
	greatpersonsrow <- horRow reveal _greatPersonCardRevealed greatPersonRoute _greatPerson _playerGreatPersonCards
	culturecardsrow <- horRow reveal _cultureCardRevealed cultureRoute _cultureCardEvent _playerCultureCards
	policyrow <- horRow True (const True) (\ pol _ -> policyRoute pol) id (snd _playerPolicies)
	techtree <- techTree di (playername,player)
	items <- itemTokens di playername player reveal
	unitcolumn <- unitColumn di (playername,player)
	pdial <- dial di playername player

	return [whamlet|
<div .NoSpacing.PlayerArea class="#{show playerarea_ori}">
  <table .NoSpacing style="border: 10px solid #{show _playerColour};">
    <tr>
      <td>
        <table .NoSpacing>
          <tr>
            <td colspan=2 align=center>
              <table>
                <tr .PlayerArea-Cards>
                  <td .PlayerArea-Cards-GreatPersons style="valign:top; align:right">
                    ^{greatpersonsrow}
                  <td .PlayerArea-Cards-Culture style="valign:top; align:left">
                    ^{culturecardsrow}
          <tr>
            <td .PlayerArea-TechArea valign=bottom .Parent .NoSpacing>
              ^{techtree}
            <td>
              <table>
                <tr>
                  <td>
                    <table style="max-width:100%">
                      <tr>
                        <td .PlayerArea-Items align=left>
                          ^{items}
                        <td .PlayerArea-Policies align=right>
                          ^{policyrow}
                <tr>
                  <td .PlayerArea-Dial>
                    ^{pdial}
      <td .PlayerArea-Units valign=top>
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

itemTokens :: DisplayInfo -> PlayerName -> Player -> Bool -> Handler Widget
itemTokens di playername (player@Player{..}) reveal = return [whamlet|
<div>
  $forall (route,datasource) <- routes
    <img style="float:left" data-source=#{data2markup datasource} src=@{route}>
|]
	where
	routes =
		map (\ r -> (resourceRoute r,ResourceSource playername r)) _playerResources ++
		map (\ h -> ((if reveal then revealedHutRoute else const hutRoute) h,HutSource playername h)) _playerHuts ++
		map (\ v -> ((if reveal then revealedVillageRoute else const villageRoute) v,VillageSource playername v)) _playerVillages ++
		map (\ a -> (artifactRoute a,ArtifactSource playername a)) _playerArtifacts

dial :: DisplayInfo -> PlayerName -> Player -> Handler Widget
dial di playername player@(Player{..}) = do
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
            <td><img .Parent style="float:left" data-source=#{data2markup $ DialCoinSource playername} src=@{route}>
    <div .Child style="left:180px; top:315px">
      <table .Parent .NoSpacing>
        <tr>
          $forall route <- culturerow
            <td><img .Parent style="float:left;" data-source=#{data2markup $ DialCultureSource playername} src=@{route}>
|]

unitColumn :: DisplayInfo -> (PlayerName,Player) -> Handler Widget
unitColumn di@(DisplayInfo{..}) (playername,player@(Player{..})) = do
	let
		reveal = isNothing myPlayerNameDI || (Just playername == myPlayerNameDI)
		unitlevel = getValueAbility1 unitLevel player
		unitcards = map (\ uc@(UnitCard{..}) -> (uc,unit2Ori (fromJust $ unitlevel unitType))) $ sort _playerUnits
	return [whamlet|
<table>
  $forall (unitcard,ori) <- unitcards
    <tr>
      <td>
        <div style="height:44px; width:205px; overflow:visible" >
          <img class="#{show ori}" style="transform-origin: 50% 50%" src=@{unitCardRoute unitcard reveal}>
|]

techCoinRow :: PlayerName -> Tech -> Coins -> Widget
techCoinRow playername tech coins = [whamlet|
<table .NoSpacing>
  <tr>
    $forall route <- coinroutes
      <td><img data-source=#{data2markup $ TechCoinSource playername tech} src=@{route}>
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
  <div .PlayerArea-Techtree.Parent.NoSpacing>
    <table border=0>
      <colgroup>
        $forall j <- columns
          <col width=94px>
      $forall (i,techcards) <- techss
        <tr>
          $forall j <- replicate i 0
            <td>
          $forall techcard@TechCard{..} <- techcards
            <td colspan=2>
              <div .TechTree-Item>
                <div .TechTree-Item-TechCard>
                  <img data-target=#{data2markup $ TechTarget playername _techCardTechId} src=@{techRoute _techCardTechId}>
                <div .TechTree-Item-Coin>
                  ^{techCoinRow playername _techCardTechId _techCardCoins}
  $if startplayer
    <img style="position:absolute; left:5px; top:0px;" src=@{startPlayerRoute}>
  <div style="position:absolute; right:5px; top:0px;">
    <table .NoSpacing>
      <tr>
        <td valign=top>
          <table .PlayerArea-Wagons.NoSpacing.>
            $forall i <- unusedwagons
              <tr><td><img .Wagon data-source=#{data2markup $ FigureSource playername Wagon} src=@{wagonRoute _playerColour}>
        <td valign=top>
          <table .PlayerArea-Flags.NoSpacing>
            $forall i <- unusedflags
              <tr><td><img .Flag data-source=#{data2markup $ FigureSource playername Flag} src=@{flagRoute _playerColour}>
        <td valign=top>
          $forall _ <- leftcities 
            <img .Debug-DragCity data-source=#{data2markup $ CitySource playername} src=@{cityRoute' (False,NoWalls,_playerColour)}>
        <td valign=top>
          $if showmetropolis
            <img .Debug-DragCity data-source=#{data2markup $ MetropolisSource playername} src=@{metropolisRoute' (NoWalls,_playerColour)}>
|]

boardArea :: DisplayInfo -> [Move] -> Handler Widget
boardArea (DisplayInfo{..}) moves = do
	let
		arr = _gameBoard gameDI
		arrlookup coors = arr!coors
		allcoors = indices arr
		(xcoors,ycoors) = (map xCoor allcoors,map yCoor allcoors)
		xs = [(minimum xcoors)..(maximum xcoors)]
		ys = [(minimum ycoors)..(maximum ycoors)]
		cityori (city@City{..}) = case _cityMetropolisOrientation of
			Nothing -> pori
			Just metropolisori -> addOri metropolisori Westward
			where
			pori = playerori _cityOwner

		rowcolspan :: Coors -> Maybe (Int,Int,String)
		rowcolspan coors = case arrlookup coors of
			Square _ _ _ _ _ (Just (CityMarker city)) _ -> case city of
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
                <td .SquareContainer .Map-SquareContainer data-source=#{data2markup $ SquareSource (Coors x y)} data-target=#{movelisttarget2markup $ filter (coors2action (Coors x y)) moves} rowspan="#{show rowspan}" colspan="#{show colspan}" alt="alt" title="#{(++) (show (x,y)) (show square)}" style="position:relative">
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
                      ^{figuresSquare playernameToPlayerDI (_squareFigures square) (Coors x y)}

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


partialDebugArea :: DisplayInfo -> Handler Widget
partialDebugArea di@(DisplayInfo{..}) = do
	return [whamlet|
<div .Debug>
|]

figuresSquare :: (PlayerName -> Player) -> SquareFigures -> Widget
figuresSquare playername2player squarefigures = [whamlet|
<div>
  $forall ((x,y),(playername,figureid,Figure{..})) <- zip this_poss figures
    <img src=@{figureRoute figure colour} style="left:#{showcoor x}px; top:#{showcoor y}px; transform: translate(-50%,-50%); position:absolute" data-source=#{data2markup $ FigureOnBoardSource figureid playername figureCoors} data-target=#{data2markup $ FigureOnBoardTarget figureid playername figureCoors}>
|]
	where
	this_poss = pos!!n
	showcoor c = show $ ((round $ (c * fromInteger 94)) :: Int)
	n = length figures
	figures = map (\ (pn,figid) -> (pn,figid,fromJust $ Map.lookup figid (_playerFiguresOnBoard $ playername2player pn)))
		squarefigures
	cfi x r = ceiling (fromIntegral x / r) :: Int
	n1 = cfi n 3
	n2 = cfi (n-n1) 2
	n3 = n-n1-n2
	pos = [
		[],
		[(0.5,0.5)],
		[(0.33,0.33),(0.67,0.67)],
		[(0.5,0.33),(0.30,0.67),(0.7,0.67)],
		[(0.3,0.33),(0.7,0.33),(0.3,0.67),(0.7,0.67)],
		[(0.3,0.33),(0.7,0.33),(0.2,0.67),(0.5,0.67),(0.8,0.67)],
		[(0.2,0.33),(0.5,0.33),(0.8,0.33),(0.2,0.67),(0.5,0.67),(0.8,0.67)],
		[(0.33,0.2),(0.67,0.2),(0.33,0.5),(0.67,0.5),(0.2,0.8),(0.5,0.8),(0.8,0.8)],
		[(0.33,0.2),(0.67,0.2),(0.3,0.5),(0.5,0.5),(0.8,0.5),(0.2,0.8),(0.5,0.8),(0.8,0.8)],
		[(0.2,0.2),(0.5,0.2),(0.8,0.2),(0.2,0.5),(0.5,0.5),(0.8,0.5),(0.2,0.8),(0.5,0.8),(0.8,0.8)] ]
		-- TODO: Expand
