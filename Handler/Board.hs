module Handler.Board where

import Import

type Coors = (Int,Int)

data Orientation = Northward | Southward | Eastward | Westward

data BoardTile = BoardTile {
	boardTileDiscovered  :: Bool,
	boardTileID          :: TileID,
	boardTileOrientation :: Orientation }

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	StartTile CivID

data CivID =
	America | Arabs | Aztec | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving Show

data Game = Game {
	gameBoard      :: Map Coors BoardTile }
--	gamePlayers    :: [PlayerID],   -- First player is start player
--	gamePhase      :: Phase,
--	gamePlayerTurn :: PlayerIndex,
--	gamePlayerData :: Map PlayerID PlayerData
--	}


{-
type Tiles = Map TileID Tile

tiles :: Tiles
tiles = 

data Tile = Tile {
	tileSquares :: Map Coors Square }

data Square = Square {
	squareTerrain :: Terrain,
	squareCity :: Maybe (PlayerID,CityID),
	squareUnits :: [(PlayerID,Piece)],
	squareResource :: Maybe Resource,
	squareCulture :: Int,
	squareCoin :: Int }

type FlagID = Int
type SettlerID = Int

data Piece = Flag FlagID | Settler SettlerID

type CityID = Int

data Terrain = Grassland | Woods | Sea | Desert | Mountain

data Resource = Incense | Iron | Cloth | Wheat | Atom | Spy

data Colour = Red | Green | Blue | Violet | Yellow

type PlayerID = Int

data Player = Player {
	playerName   :: String,
	playerCiv    :: CivID,
	playerColour :: Colour }

type PlayerIndex = Int

data Phase = StartOfTurn | Trade | CityManagement | Movement | Research
	deriving (Show,Eq,Ord)

data PlayerData = PlayerData {
	freeCoins :: Int,
	freeCulture :: Int,
	dialTrade :: Int,
	techTree :: TechTree,
	playerGovernment :: Government,
	playerPolicies :: [Policy] }

data Policy =
	Rationalism | NaturalReligion | MilitaryTradition | UrbanDevelopment |
	Patronage | Pacifism | OrganizedReligion | Expansionism
	deriving (Show,Eq)

type TechLevel = Int

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	CivilService | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamPower | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplaceableParts | Flight | Plastics | Combustion | AtomicTheory |
	SpaceFlight
	deriving (Show,Eq)

data Building =
	Market | Bank | Temple | Cathedral | Granary | Aquaeduct | Library | University |
	Barracks | Academy | TradingPost | Workshop | IronMine | Harbour | Shipyard

data Unit =
	Infantry_1_3 | Infantry_2_2 | Infantry_3_1 |
	Artillery_1_3 | Artillery_2_2 | Artillery_3_1 |
	Cavalry_1_3 | Cavalry_2_2 | Cavalry_3_1 |
	Aircraft_5_7 | Aircraft_6_6 | Aircraft_7_5

data Wonder =
	Stonehenge | Colossus | HangingGardens | TheOracle | TheGreatWall |
	ChichenItza | Pyramids | GreatLighthouse | StatueOfZeus |
	AngkorWat | HimejiCastle | TajMahal | PorcelainTower | MachuPichu |
	BrandenburgGate | Louvre | NotreDame | LeonardosWorkshop |
	SydneyOperaHouse | StatueOfLiberty | PanamaCanal | UnitedNations |
	BigBen | CristoRedentor | Kremlin | Pentagon | TheInternet
	deriving (Show,Eq)

data CityState = CityState1 | CityState2 | CityState3 | CityState4 | CityState5

type TechTree = Map TechLevel [Tech]

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Eq)

class Outskirts a where
	oCoins :: Int
	oHammers :: Int
	oTrade :: Int
	oCulture :: Int
	

class Abilities a where
	aCoins :: Maybe Int
	aCultureStartTurn :: Maybe Int
	aCultureOutskirts :: Maybe Int
	aHammersOutskirts :: Maybe Int
	aTradeOutskirts
	aTradeStartTurn :: Maybe Int
	aEnablesBuildings :: Maybe Building
	aEnablesWonder :: Maybe Wonder
	aEnablesGovernment :: Maybe Government
	aMilitaryAdvantage :: Maybe Int
	aPieceLimit :: Maybe Int
	aPieceLimitIncrease :: Maybe Int
-}