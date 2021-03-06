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
import Data.List ((!!),nub)

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
import AssocList

default (Int, Float)


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

queryUpdateCivH event = do
	app <- getYesod
	update' (appCivAcid app) event

displayGame :: (UserId,User,GameName,Game,Maybe PlayerName) -> Handler Html
displayGame (userid,user,gamename,game,mb_playername) = do
	moves <- case mb_playername of
		Nothing -> return []
		Just playername -> queryUpdateCivH $ MoveGen gamename playername 
	let
		toplayer playername = fromJust $ lookupAssocList playername (_gamePlayers game)
		mb_myplayer = fmap toplayer mb_playername
		myplayerori = maybe Northward _playerOrientation mb_myplayer
		di = DisplayInfo gamename game mb_playername mb_myplayer myplayerori toplayer
		(playernametomove,_) = nthAssocList (_gamePlayersTurn game) (_gamePlayers game)
	movelist <- moveList di
	playerlist <- playerList di
	boardarea <- boardArea di moves
	actionarea <- actionArea di mb_playername moves
	debugarea <- partialDebugArea di
	overviewboard <- overviewBoard di
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
<div .GameArea>
  <div .Arena>^{arena}
  <div .ArenaZoom.Debug-Draggable>
  <div .MapTest.Action-HideMapTest>
    <div .MapTest2>MapTest
  <div .DragArea>
  <div .Overview>^{overviewboard}

<div .Sidebar>
  <table>
    <tr><td>^{movelist}
    <tr><td>Zoom <input type="text" id="Zoom" readonly style="border:0;" /><div id="ZoomSlider" />
    <tr><td>^{playerlist}
    <tr><td>^{actionarea}
    <tr><td><a href="#" class="Action-OpenDebug">Show Debug
    <tr><td><a href="#" class="Action-OpenOverview">Show Overview
    <tr><td><a href="#" class="Action-ShowMapTest">Show Map
    <tr><td><a href="#" class="Action-ShowDialog" onclick="showDialog()">Show Dialog
    <tr><td><a href=@{HomeR}>Change

<div id="Dialog-Test" title="TestDialog">
  <p>
    <span class="ui-icon ui-icon-alert" style="float:left; margin:0 7px 20px 0;">
    Click <a href=@{HomeR}>here</a> to switch to the next player
  <p>

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

moveList :: DisplayInfo -> Handler Widget
moveList di@(DisplayInfo{..}) = do
	let moves = map (\(turn,phs)->(turn,fromAssocList phs)) $ fromAssocList $ _gameMoves gameDI
	return [whamlet|
<div style="overflow:scroll">
  <ul .collapsibleList>
    $forall (turn,phasemoves) <- moves
      <li>
        $with labelprefix <- concat ["movelist_turn",show turn]
          <label for=#{labelprefix}>Turn #{show turn}
          <input type="checkbox" id=#{labelprefix}>
          <ul>
            $forall (i,(phase,movenodes)) <- zip iinf phasemoves
              <li>
                $with labelprefix2 <- concat [labelprefix,"_",show i]
                  <label for=#{labelprefix2}>#{show phase}
                  <input type="checkbox" id=#{labelprefix2}>
                  ^{movenodes2html labelprefix2 movenodes}
|]
	where
	iinf = [1..] :: [Int]
	movenodes2html labelprefix movenodes = [whamlet|
<ul>
  $forall (i,movenode) <- zip iinf movenodes
    <li>
      $case movenode
        $of NormalMove pn move
          #{concat [Text.unpack (playerName pn),": ",show move]}
        $of SubPhaseMoves subphase submovenodes
          $with labelprefix2 <- concat [labelprefix,"_",show i]
            <label for=#{labelprefix2}>#{subphaseName subphase}
            <input type="checkbox" id=#{labelprefix2}>
            ^{movenodes2html labelprefix2 submovenodes}
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
dial di@(DisplayInfo{..}) playername player@(Player{..}) = do
	Coins coins <- queryUpdateCivH $ PlayerNumCoins gameNameDI playername
	let
		tradeangle = div ((tradeTrade _playerTrade - 1) * 360) 28
		coinangle = tradeangle + div ((coins - 1) * 360) 16
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
		projecttechlevel (i,level) = ( i, maybe [] id (Map.lookup level _playerTechs) )
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
boardArea di@(DisplayInfo{..}) moves = do
	let
		arr = _gameBoard gameDI
		arrlookup coors = arr!coors
		allcoors = indices arr
		(xcoors,ycoors) = (map xCoor allcoors,map yCoor allcoors)
		xs = [(minimum xcoors)..(maximum xcoors)]
		ys = [(minimum ycoors)..(maximum ycoors)]

		playerori playername = _playerOrientation $ playernameToPlayerDI playername
		playercolour playername = _playerColour $ playernameToPlayerDI playername

		showcity :: Coors -> Maybe (City,String)
		showcity coors = case arrlookup coors of
			Square _ _ _ _ _ (Just (CityMarker citysq)) _ -> case citysq of
				SecondCitySquare ori -> case ori of
					Westward  -> Nothing
					Northward -> Nothing
					_ -> Just (city,cityori city ++ "Metropolis") where
						city@(City _ _ _ _ _ _ _ _) = _cityMarker $ fromJust $ _squareTokenMarker $ arrlookup (addCoorsOri coors ori)
				city@(City _ _ _ _ _ _ _ Nothing) -> Just (city,cityori city ++ "Square")
				city@(City _ _ _ _ _ _ _ (Just ori)) -> case ori of
					Westward  -> Nothing
					Northward -> Nothing
					_  -> Just (city,cityori city ++ "Metropolis")
			_ -> Nothing
			where
			cityori City{..} = show $ case _cityMetropolisOrientation of
				Nothing -> playerori _cityOwner
				Just metropolisori -> case (playerori _cityOwner,metropolisori) of
					(Northward,Northward) -> Westward
					(Northward,Southward) -> Westward
					(Northward,Eastward)  -> Northward
					(Northward,Westward)  -> Northward
					(Westward, Northward) -> Westward
					(Westward, Southward) -> Westward
					(Westward, Westward)  -> Southward
					(Westward, Eastward)  -> Southward
					(Southward,Northward) -> Eastward
					(Southward,Southward) -> Eastward
					(Southward,Westward)  -> Southward
					(Southward,Eastward)  -> Southward
					(Eastward, Northward) -> Eastward
					(Eastward, Southward) -> Eastward
					(Eastward, Westward)  -> Northward
					(Eastward, Eastward)  -> Northward
	return [whamlet|
<div .Parent>
  <div .Child .Map-Layer1>
    <table .NoSpacing>
      <tr>
        $forall x <- xs
          <td .Map-Col>
      $forall y <- ys
        <tr>
          $forall x <- xs
            $with square <- arrlookup (Coors x y)
                <td .SquareContainer .Map-SquareContainer style="overflow:visible" data-source=#{data2markup $ SquareSource (Coors x y)} data-target=#{data2markup $ SquareTarget (Coors x y)} alt="alt" title="#{(++) (show (x,y)) (show square)}" style="position:relative">
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
                          $of WonderMarker wonder
                            <img .Center class="#{show myPlayerOriDI}" src=@{wonderBuildingRoute wonder}>
                          $of BuildingMarker (Building buildingtype owner)
                            <img .Center class="#{show (playerori owner)}Square" src=@{buildingTypeRoute buildingtype}>
                          $of CityMarker _
                            $maybe (city,oriclass) <- showcity (Coors x y)
                              <div style="overflow:visible">
                                <div style="overflow:visible" .Center>
                                  <img class="#{show oriclass}" src=@{cityRoute (playercolour (_cityOwner city)) city}>
                          $of _
                      ^{figuresSquare di (_squareFigures square)}

  <div .Map-Layer2>
    <table .NoSpacing>
      <tr>
        $forall x <- xs
          <td .Map-Col>
      $forall y <- ys
        <tr .Map-Row>
          $forall x <- xs
            $case arrlookup (Coors x y)
              $of OutOfBounds
                <td .TileContainer-OutOfBounds><img .Center src=@{transparentSquareRoute}> 
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

figuresSquare :: DisplayInfo -> SquareFigures -> Widget
figuresSquare di@(DisplayInfo{..}) squarefigures = [whamlet|
<div>
  $forall ((x,y),(playername,figureid,figure@(Figure{..}))) <- zip this_poss figures
    <img .PlayerAction src=@{figureRoute _figureType (_playerColour $ playernameToPlayerDI playername)} style="left:#{showcoor x}px; top:#{showcoor y}px; transform: translate(-50%,-50%); position:absolute" data-source=#{data2markup $ FigureOnBoardSource figureid playername _figureCoors} data-target=#{data2markup $ FigureOnBoardTarget figureid playername _figureCoors} alt="alt" title="#{show figure}">
|]
	where
	this_poss = pos!!n
	showcoor c = show $ ((round $ (c * fromInteger 94)) :: Int)
	n = length figures
	figures = map (\ (pn,figid) -> (pn,figid,fromJust $ Map.lookup figid (_playerFiguresOnBoard $ playernameToPlayerDI pn)))
		squarefigures
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
		[(0.2,0.2),(0.5,0.2),(0.8,0.2),(0.2,0.5),(0.5,0.5),(0.8,0.5),(0.2,0.8),(0.5,0.8),(0.8,0.8)] ] ++
		repeat (error "figuresSquare pos too many figures, not implemented yet")
		-- TODO: Expand

stackOfRoutes :: String -> Int -> Int -> Int -> Int -> [Route App] -> Maybe ActionSource -> Maybe ActionTarget -> Widget
stackOfRoutes classstr x0 y0 xd yd routes mb_datasource mb_datatarget = let
	style x y z = "position:absolute; top:" ++ show y ++ "px; left:" ++ show x ++ "px; z-index:" ++ show z
	rs = [ (route,(x0 + i*xd, y0 + i*yd, 10+i)) | (i,route) <- zip [0..] routes ]
	source = maybe (NoSource ()) id mb_datasource
	target = maybe (NoTarget ()) id mb_datatarget
	in [whamlet|
<div .Child .PlayerAction style="position:relative" class=#{show classstr} data-source=#{data2markup $ source} data-target=#{data2markup $ target}>
  $forall (route,(x,y,z)) <- rs
    <img .Child src=@{route} style=#{style x y z}>
|]

overviewBoard di@DisplayInfo{..} = do
	let
		Game{..} = gameDI
		(dxu,dyu) = (3,3)
		(dxb,dyb) = (5,3)
		playercolour playername = _playerColour $ playernameToPlayerDI playername

		wondercardroutes = map wonderCardRoute _gameOpenWonders
		iwonders = zip [0..] _gameOpenWonders
		iwondery i = 155 + i*135
		wonderstackroutes = map (wonderBackRoute.wonderLevel) (reverse $ concat $ tokenStackElems _gameWonderStack)
		buildingroutes buildingtype = replicate (length buildingmarkers) $ buildingTypeRoute buildingtype
			where
			Just buildingmarkers = tokenStackLookup (buildingTypeToMarker buildingtype) _gameBuildingStack
		unitroutes unittype = replicate (length unitcards) unitCardBackRoute
			where
			Just unitcards = tokenStackLookup unittype _gameUnitStack

		playersteps = [ (steps,map ((figureRoute Flag).playercolour.fst) $
			filter ((==steps)._playerCultureSteps.snd) (fromAssocList _gamePlayers)) |
				steps <- nub $ map _playerCultureSteps $ assocListValues _gamePlayers ]
		stepsx steps = 9 + steps*71

	return [whamlet|
<div .Parent.Action-CloseOverview name="overviewboard">
  <div .Child>
    ^{stackOfRoutes "" 10 139 0 135 wondercardroutes Nothing Nothing}
    $forall (i,wonder) <- iwonders
      ^{stackOfRoutes "Eastward" 209 (iwondery i) 10 10 [wonderBuildingRoute wonder] (Just $ ProductionSource $ ProduceWonder wonder) Nothing}
    ^{stackOfRoutes ""   10   4 dxu dyu wonderstackroutes Nothing Nothing}
    ^{stackOfRoutes ""  357  20 dxb dyb (buildingroutes Market) (Just $ ProductionSource $ ProduceBuilding MarketOrBank) Nothing }
    ^{stackOfRoutes ""  357 184 dxb dyb (buildingroutes Granary) (Just $ ProductionSource $ ProduceBuilding GranaryOrAquaeduct) Nothing }
    ^{stackOfRoutes ""  357 347 dxb dyb (buildingroutes Barracks) (Just $ ProductionSource $ ProduceBuilding BarracksOrAcademy) Nothing }
    ^{stackOfRoutes ""  410 512 dxb dyb (buildingroutes TradePost) (Just $ ProductionSource $ ProduceBuilding TradePosts) Nothing }
    ^{stackOfRoutes ""  603  20 dxb dyb (buildingroutes Temple) (Just $ ProductionSource $ ProduceBuilding TempleOrCathedral) Nothing }
    ^{stackOfRoutes ""  603 184 dxb dyb (buildingroutes Library) (Just $ ProductionSource $ ProduceBuilding LibraryOrUniversity) Nothing }
    ^{stackOfRoutes ""  603 347 dxb dyb (buildingroutes Forge) (Just $ ProductionSource $ ProduceBuilding ForgeOrIronMine) Nothing }
    ^{stackOfRoutes ""  603 512 dxb dyb (buildingroutes Harbour) (Just $ ProductionSource $ ProduceBuilding Harbours) Nothing }
    ^{stackOfRoutes ""  713 512 dxb dyb (buildingroutes Shipyard) (Just $ ProductionSource $ ProduceBuilding Shipyards) Nothing }
    ^{stackOfRoutes "" 1013  63 dxu dyu (unitroutes Infantry) (Just $ ProductionSource $ ProduceUnit Infantry) Nothing }
    ^{stackOfRoutes "" 1399  63 dxu dyu (unitroutes Cavalry) (Just $ ProductionSource $ ProduceUnit Cavalry) Nothing }
    ^{stackOfRoutes "" 1013 398 dxu dyu (unitroutes Artillery) (Just $ ProductionSource $ ProduceUnit Artillery) Nothing }
    ^{stackOfRoutes "" 1399 398 dxu dyu (unitroutes Aircraft) (Just $ ProductionSource $ ProduceUnit Aircraft) Nothing }
    $forall (steps,playerroutes) <- playersteps
      <div .Center>^{stackOfRoutes "" (stepsx steps) 712 0 20 playerroutes Nothing Nothing}
  <div .Child>
    <img .Child src=@{overviewRoute} alt="alt" title=#{show $ _gameMoves}>
|]