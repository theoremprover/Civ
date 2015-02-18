{-# LANGUAGE TemplateHaskell #-}

module Handler.Board2 where

import Import

import Database.Persist.TH

data Orientation = Northward | Southward | Eastward | Westward
	deriving (Show,Read,Eq)
derivePersistField "Orientation"

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	TileAmerica | TileArabs | TileAztecs | TileChina | TileEgypt | TileEnglish | TileFrench | 
	TileGermany | TileGreeks | TileIndians | TileJapanese | TileMongols | TileRome | 
	TileRussia | TileSpanish | TileZulu
	deriving (Show,Read,Eq)
derivePersistField "TileID"

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Read,Eq)
derivePersistField "Civ"

data Terrain = Grass | Woods | Sea | Desert | Mountain
	deriving (Show,Read,Eq)
derivePersistField "Terrain"

data Resource = Incense | Iron | Cloth | Wheat | Atom | Spy
	deriving (Show,Read,Eq)
derivePersistField "Resource"

data Colour = Red | Green | Blue | Violet | Yellow
	deriving (Show,Read,Eq)
derivePersistField "Colour"

data Phase = StartOfTurn | Trade | CityManagement | Movement | Research
	deriving (Show,Read,Eq,Ord)
derivePersistField "Phase"

data Policy =
	Rationalism | NaturalReligion | MilitaryTradition | UrbanDevelopment |
	Patronage | Pacifism | OrganizedReligion | Expansionism
	deriving (Show,Read,Eq)
derivePersistField "Policy"

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	CivilService | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamPower | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplaceableParts | Flight | Plastics | Combustion | AtomicTheory |
	SpaceFlight
	deriving (Show,Read,Eq)
derivePersistField "Tech"

data Building =
	Market | Bank | Temple | Cathedral | Granary | Aquaeduct | Library | University |
	Barracks | Academy | TradingPost | Workshop | IronMine | Harbour | Shipyard
	deriving (Show,Read,Eq)
derivePersistField "Building"

data Unit =
	Infantry_1_3 | Infantry_2_2 | Infantry_3_1 |
	Artillery_1_3 | Artillery_2_2 | Artillery_3_1 |
	Cavalry_1_3 | Cavalry_2_2 | Cavalry_3_1 |
	Aircraft_5_7 | Aircraft_6_6 | Aircraft_7_5
	deriving (Show,Read,Eq)
derivePersistField "Unit"

data Wonder =
	Stonehenge | Colossus | HangingGardens | TheOracle | TheGreatWall |
	ChichenItza | Pyramids | GreatLighthouse | StatueOfZeus |
	AngkorWat | HimejiCastle | TajMahal | PorcelainTower | MachuPichu |
	BrandenburgGate | Louvre | NotreDame | LeonardosWorkshop |
	SydneyOperaHouse | StatueOfLiberty | PanamaCanal | UnitedNations |
	BigBen | CristoRedentor | Kremlin | Pentagon | TheInternet
	deriving (Show,Read,Eq)
derivePersistField "Wonder"

data CityState = CityState1 | CityState2 | CityState3 | CityState4 | CityState5
	deriving (Show,Read,Eq)
derivePersistField "CityState"

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Read,Eq)
derivePersistField "Government"

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Read,Eq,Ord,Enum)
derivePersistField "TechLevel"
