{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Handler.StaticResources where

import Import

import Handler.MakeRoutes

import Model

$(makeMultiRoutes [''TileID] "tileRoute" ["_Tiles_","_jpg"])
$(makeMultiRoutes [''Civ] "tileRouteBack" ["_Tiles_Tile_","_back_jpg"])
boardTileRoute tileid True = tileRoute tileid
boardTileRoute tileid False = case tileid of
	Tile civ -> tileRouteBack civ
	_        -> StaticR $ _Tiles_Back_jpg

$(makeMultiRoutes [''Civ] "dialRoute" ["_Dials_","_jpg"])
tradeDialRoute = StaticR $ _Dials_Tradedial2_gif
coinDialRoute = StaticR $ _Dials_Coindial2_gif
oneCultureRoute = StaticR $ _Dials_1Culture_gif
fiveCultureRoute = StaticR $ _Dials_5Culture_gif
coinRoute = StaticR $ _Dials_Coin_gif

$(makeMultiRoutes [''CultureEvent] "cultureRouteRevealed" ["_Culture_","_jpg"])
cultureRoute ev False = StaticR $ case cultureEventLevel ev of
	CultureLevelI   -> _Culture_CultureLevel1_back_jpg
	CultureLevelII  -> _Culture_CultureLevel2_back_jpg
	CultureLevelIII -> _Culture_CultureLevel3_back_jpg
cultureRoute ev True = cultureRouteRevealed ev

$(makeMultiRoutes [''Tech] "techRoute" ["_Techs_","_jpg"])

transparentSquareRoute = StaticR _Tiles_TransparentSquare_gif

$(makeMultiRoutes [''GreatPerson] "greatPersonRouteRevealed" ["_Great_","_jpg"])
greatPersonRoute greatperson revealed = case revealed of
	False -> StaticR _Great_Back_gif
	True  -> greatPersonRouteRevealed greatperson

$(makeMultiRoutes [''Terrain] "terrainRoute" ["_Squares_","_jpg"])

$(makeMultiRoutes [''Colour] "colourRouteFlag" ["_Figures_Flag_","_gif"])
$(makeMultiRoutes [''Colour] "colourRouteWagon" ["_Figures_Wagon_","_gif"])
figureRoute figure colour = case figure of
	Flag  -> colourRouteFlag colour
	Wagon -> colourRouteWagon colour

$(makeMultiRoutes [''Investment] "investmentRoute" ["_Investments_","_jpg"])

$(makeMultiRoutes [''UnitType,''UnitBalance] "unitCardRouteRevealed" ["_Units_","_","_jpg"])
unitCardRoute (UnitCard{..}) revealed = case revealed of
	True  -> unitCardRouteRevealed (unitType,unitBalance)
	False -> StaticR _Units_Unit_back_jpg

$(makeMultiRoutes [''Wonder] "wonderBuildingRoute" ["_Squares_","_Building_jpg"])
$(makeMultiRoutes [''Wonder] "wonderCardRoute" ["_Wonders_","_jpg"])

$(makeMultiRoutes [''BuildingType] "buildingTypeRoute" ["_Squares_","_jpg"])

$(makeMultiRoutes [''Resource] "resourceRoute" ["_Resources_","_gif"])

$(makeMultiRoutes [''Hut] "revealedHutRoute" ["_Resources_","_gif"])
hutRoute = StaticR _Resources_Hut_gif

$(makeMultiRoutes [''Village] "revealedVillageRoute" ["_Resources_","_gif"])
villageRoute = StaticR _Resources_Village_gif

$(makeMultiRoutes [''Artifact] "artifactRoute" ["_Squares_","_jpg"])

$(makeMultiRoutes [''Policy] "policyRoute" ["_Policies_","_jpg"])

$(makeMultiRoutes [''Government] "governmentRoute" ["_Policies_","_jpg"])

$(makeMultiRoutes [''CityState] "cityStateRoute" ["_Squares_","_jpg"])

$(makeMultiRoutes [''Bool,''Walls,''Colour] "cityRoute'" ["_Squares_CityT_","_","_","_jpg"])
$(makeMultiRoutes [''Walls,''Colour] "metropolisRoute'" ["_Squares_MetropolisT_","_","_jpg"])
cityRoute colour (City{..}) = case _cityMetropolisOrientation of
	Nothing  -> cityRoute' (_cityCapital,_cityWalls,colour)
	Just ori -> metropolisRoute' (_cityWalls,colour)

startPlayerRoute = StaticR _Table_StartPlayer_gif

$(makeMultiRoutes [''Colour] "wagonRoute" ["_Figures_Wagon_","_gif"])
$(makeMultiRoutes [''Colour] "flagRoute" ["_Figures_Flag_","_gif"])

overviewRoute = StaticR _Table_Overview_jpg
