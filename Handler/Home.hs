{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Home where


import Import

import Database.Persist.Sqlite

import Handler.Board
import Handler.Board2
import Handler.BoardDisplay

import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Prelude

getHomeR :: Handler Html
getHomeR = do
	game <- runDB $ do
		gid <- insert $ Game [
			BoardTile TileSpanish 0 0 True Southward,
			BoardTile Tile1 4 0 True Eastward,
			BoardTile Tile2 0 4 True Southward,
			BoardTile Tile3 4 4 False Southward,
			BoardTile Tile4 0 8 False Southward,
			BoardTile Tile5 4 8 True Northward,
			BoardTile Tile6 0 12 True Westward,
			BoardTile TileArabs 4 12 True Northward ]
			[
				Player "Red" English Red 2 7 15 Democracy [Rationalism,NaturalReligion]
					[TechCard TechLevelI Writing 1, TechCard TechLevelI HorsebackRiding 0, TechCard TechLevelII DemocracyTech 2]
					[ Artillery_1_3,Infantry_2_2,Cavalry_1_3 ]
					[	CultureCard Sabotage False,
						CultureCard SharedKnowledge True ]
					[	GreatPersonCard JoanOfArc True,
						GreatPersonCard KhalidIbnAlWalid True,
						GreatPersonCard Leonidas False,
						GreatPersonCard LorenzoDiMedici False ],
				Player "Green" Russia Green 3 29 16 Despotism [UrbanDevelopment,NaturalReligion,MilitaryTradition]
					[TechCard TechLevelI Metalworking 0, TechCard TechLevelI HorsebackRiding 0 ]
					[ Infantry_2_2,Infantry_2_2,Artillery_1_3,Artillery_3_1,Infantry_3_1,Aircraft_7_5,Aircraft_5_7 ]
					[	CultureCard GenerousGift False,
						CultureCard GiftFromAfar True,
						CultureCard HonorAndDuty False,
						CultureCard Ideas False,
						CultureCard Immigrants False ]
					[	GreatPersonCard APGianni False,
						GreatPersonCard Archimedes True ]
				]
			StartOfTurn 0

		insert $ Games "testgame" gid
		get404 gid
	
	defaultLayout $ do
		setTitle "Civilization Boardgame"
		let
			di = displayInfoFactory 0.75 game
		$(widgetFile "homepage")
