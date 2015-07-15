{-# LANGUAGE
	CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Model where

import Prelude
import Data.Data
import Data.Ix
import Data.Text (Text(..))
import Data.Typeable
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.List
import Data.Ord
import Data.Time
import Control.Lens
import qualified Data.Map as Map
--import Data.Array.IArray

import Entities
import TokenStack

import qualified Data.Ix as Ix

allOfThem :: (Ix t,Bounded t) => [t]
allOfThem = range (minBound,maxBound)

replicateUnit (t,n) = replicate () n
replicateT (t,n) = replicate n t

data Coors = Coors { xCoor :: Int, yCoor :: Int }
	deriving (Show,Read,Data,Typeable,Eq,Ord)
$(deriveSafeCopy modelVersion 'base ''Coors)

addCoors (Coors x1 y1) (Coors x2 y2) = Coors (x1+x2) (y1+y2)

newtype Trade = Trade Int deriving (Show,Read,Num,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Trade)
newtype Coins = Coins Int deriving (Show,Read,Num,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Coins)
newtype Culture = Culture Int deriving (Show,Read,Num,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Culture)

data Orientation = Northward | Eastward | Southward | Westward
	deriving (Show,Read,Eq,Ord,Ix.Ix,Bounded,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Orientation)

data Colour = Red | Green | Blue | Violet | Yellow
	deriving (Show,Read,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''Colour)

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Read,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''Civ)

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	Tile Civ
	deriving (Show,Read,Eq,Data,Ix,Bounded,Ord,Typeable)
$(deriveSafeCopy modelVersion 'base ''TileID)

data Phase = StartOfTurn | Trading | CityManagement | Movement | Research
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Phase)

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	PublicAdministration | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamEngine | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplacementParts | Flight | Plastics | CombustionEngine | AtomicTheory |
	SpaceFlight
	deriving (Show,Read,Data,Ord,Ix,Bounded,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Tech)

initialTechStack :: TokenStack () Tech
initialTechStack = tokenStackFromList [ ((),allOfThem) ]

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Read,Eq,Ord,Enum,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''TechLevel)

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Government)

data Resource = Incense | Wheat | Linen | Iron | Spy | Atom
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Resource)

data Terrain = Grassland | Desert | Mountains | Forest | Water
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Terrain)

data Artifact = AttilaVillage | Atlantis | ArkOfCovenant | SevenCitiesOfGold | SchoolOfConfucius
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Artifact)

data Hut = ResourceHut Resource | CityStateHut | ThreeCulture | TeacherHut | FriendlyBarbarianHut
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Hut)

initialHutStack :: TokenStack () Hut
initialHutStack = tokenStackFromList $ concatMap replicateT [
	(ResourceHut Spy,6),(ResourceHut Wheat,7),(ResourceHut Incense,6),
	(ResourceHut Linen,6),(ResourceHut Iron,3),(ResourceHut Atom,1),
	(CityStateHut,2),(TeacherHut,1),(ThreeCulture,1),
	(FriendlyBarbarianHut,2) ]

data Village = ResourceVillage Resource | FourHammers | SixCulture | CityStateVillage |
	CoinVillage | GreatPersonVillage
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Village)

initialVillageSet :: TokenStack () Village
initialVillageSet = tokenStackFromList $ concatMap replicateT [
	(ResourceVillage Spy,4),(ResourceVillage Atom,4),(ResourceVillage Iron,3),
	(CityStateVillage,3),(SixCulture,1),
	(FourHammers,1),(CoinVillage,2),(GreatPersonVillage,2) ]

data CityType = City | Metropolis
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''CityType)

initialCityStack :: TokenStack CityType ()
initialCityStack = tokenStackFromList $ map replicateUnit [
	(City,3),(Metropolis,1) ]

data City = SecondCitySquare Orientation | City {
	_cityOwner :: PlayerName,
	_cityType :: CityType,
	_cityDoubleProd :: Bool,
	_cityFortified :: Bool,
	_cityCaravan :: Bool,
	_cityMetropolis :: Maybe Orientation
	}
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''City)

data BuildingMarker = BarracksOrAcademy | ForgeOrForge2 |
	GranaryOrAquaeduct | TempleOrCathedral | LibraryOrUniversity |
	MarketOrBank | Harbours | TradeStations | Shipyards
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''BuildingMarker)

data Building = Barracks | Forge | Granary | Harbour | Library |
	Market | Shipyard | TradeStation | Temple |
	Academy | Aquaeduct | Bank | Cathedral | Forge2 | University
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Building)

initialBuildingStack :: TokenStack BuildingMarker ()
initialBuildingStack = tokenStackFromList $ concatMap replicateUnit [
	(BarracksOrAcademy,9),(ForgeOrForge2,9),      (GranaryOrAquaeduct,9),
	(TempleOrCathedral,9),(LibraryOrUniversity,9),(MarketOrBank,9),
	(Harbours,9),         (TradeStations,9),      (Shipyards,9) ]

data TokenMarker =
	ArtifactMarker Artifact |
	HutMarker Hut |
	VillageMarker Village |
	CityMarker City |
	BuildingMarker Building
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''TokenMarker)

initialFigureStack :: TokenStack Figure ()
initialFigureStack = tokenStackFromList $ map replicateUnit [
	(Figure,6), (Wagon,2) ]

data Figure = Flag | Wagon
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Figure)

data Square = Square {
	_squareTerrain     :: [Terrain],
	_squareCoin        :: Bool,
	_squareResource    :: Maybe Resource,
	_squareNatWonder   :: Bool,
	_squareTokenMarker :: Maybe TokenMarker,
	_squareBuilding    :: Maybe Building,
	_squareFigures     :: [Figure]
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''Square)
makeLenses ''Square

initialBoardTileStack :: TokenStack () TileID
initialBoardTileStack = tokenStackFromList [ ((),range (Tile1,Tile27)) ]

data BoardTile = BoardTile {
	_boardTileId :: TileID,
	_boardTileCoors :: Coors,
	_boardTileDiscovered :: Bool,
	_boardTileOrientation :: Maybe Orientation
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''BoardTile)
makeLenses ''BoardTile

data TechCard = TechCard {
	_techCardTechId :: Tech,
	_techCardLevel :: TechLevel,
	_techCardCoins :: Coins
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''TechCard)
makeLenses ''TechCard

newtype PlayerName = PlayerName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy modelVersion 'base ''PlayerName)

playerName (PlayerName pn) = pn

type PlayerEmail = Text

data Player = Player {
	_playerUserEmail :: PlayerEmail,
	_playerColour :: Colour,
	_playerCiv :: Civ,
	_playerGovernment :: Government,
	_playerTrade :: Trade,
	_playerCulture :: Culture,
	_playerCoins :: Coins,
	_playerTechs :: [TechCard]
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''Player)
makeLenses ''Player

makePlayer useremail colour civ = Player useremail colour civ Despotism (Trade 0) (Culture 0) (Coins 0) []

data GameState = Waiting | Running | Finished
	deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''GameState)

newtype GameName = GameName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy modelVersion 'base ''GameName)

gameName (GameName gn) = gn

type Players = [(PlayerName,Player)]

data Game = Game {
	_gameCreationDate :: UTCTime,
	_gameCreator :: UserName,
	_gameState :: GameState,
	_gameBoardTiles :: [BoardTile],
	_gamePlayers :: Players,
	_gameBoard :: Array Coors Square,
	_gameTileStack :: TokenStack () TileID
	_gameHutStack :: TokenStack () Hut,
	_gameVillageStack :: TokenStack () Village
--	_gameBuildingStack :: TokenStack BuildingMarker (),
--	_gameGreatPersonStack :: TokenStack () GreatPerson,
--	_gameUnitStack :: TokenStack UnitType Unit,
	}
	deriving (Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Game)
makeLenses ''Game

instance Eq Game where
	g1 == g2 =
		_gameCreationDate g1 == _gameCreationDate g2 &&
		_gameCreator g1 == _gameCreator g2

instance Ord Game where
	compare = comparing _gameCreationDate

type Games = Map.Map GameName Game

data CivState = CivState {
	_civGames :: Games
	}
	deriving (Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''CivState)
makeLenses ''CivState

data CultureEvent =
	Astray | BarbarianEncampment | BankCrisis | BreadCircus | Catastrophe |
	Colonists | Counterfeit | DayOfThePresident | DayOfTheTyrant | Deforestation |
	Disaster | Desertion | Drought | EconomicCrisis | ExchangeIdeas | Faithless |
	Flooding | GenerousGift | GiftFromAfar | HonorAndDuty | Ideas | Immigrants |
	KnightTournament | LongLiveTheQueen | MassDefection | Migrants | Displaced |
	Nationalism | Disoriented | Patriotism | PrimeTime | PrincelyGift | RevoltI | RevoltII |
	RoamingHoarde | Sabotage | SharedKnowledge | SupplyDrop
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''CultureEvent)

data CultureLevel = CultureLevel1 | CultureLevel2 | CultureLevel3
	deriving (Show,Read,Data,Ord,Bounded,Ix,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''CultureLevel)

cultureEventsOfLevel :: CultureLevel -> [CultureEvent]
cultureEventsOfLevel CultureLevel1 = [
	GiftFromAfar,GiftFromAfar,GiftFromAfar,
	BarbarianEncampment,BarbarianEncampment,
	BreadCircus,BreadCircus,
	Counterfeit,Counterfeit,
	Desertion,Desertion,
	Disoriented,
	DayOfTheTyrant,DayOfTheTyrant,DayOfTheTyrant,
	Drought,Drought,
	ExchangeIdeas,ExchangeIdeas,ExchangeIdeas,
	HonorAndDuty,HonorAndDuty,
	Migrants,Migrants,
	RevoltI,
	Sabotage,Sabotage ]
cultureEventsOfLevel CultureLevel2 = [
	GenerousGift,GenerousGift,GenerousGift,
	Catastrophe,
	Colonists,Colonists,
	Deforestation,Deforestation,
	EconomicCrisis,
	Flooding,
	KnightTournament,KnightTournament,
	SharedKnowledge,SharedKnowledge,SharedKnowledge,
	Astray,
	MassDefection,MassDefection,
	Nationalism,Nationalism,
	RoamingHoarde,RoamingHoarde,
	RevoltII,
	LongLiveTheQueen,LongLiveTheQueen ]
cultureEventsOfLevel CultureLevel3 = [
	PrincelyGift,PrincelyGift,
	BankCrisis,
	Disaster,
	Immigrants,Immigrants,
	Displaced,
	Patriotism,
	PrimeTime,PrimeTime,
	SupplyDrop,SupplyDrop,
	DayOfThePresident,
	MassDefection,
	Ideas,Ideas ]

cultureEventLevel ev | ev `elem` (cultureEventsOfLevel CultureLevel1) = CultureLevel1
cultureEventLevel ev | ev `elem` (cultureEventsOfLevel CultureLevel2) = CultureLevel2
cultureEventLevel ev | ev `elem` (cultureEventsOfLevel CultureLevel3) = CultureLevel3

data CultureCard = CultureCard {
	_cultureCardRevealed :: Bool,
	_cultureCardEvent    :: CultureEvent,
	_cultureCardCoins    :: Coins
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''CultureCard)
makeLenses ''CultureCard

-------- tileSquare

{-
data Square = Square {
	_squareTerrain  :: [Terrain],
	_squareCoin     :: Bool,
	_squareResource :: Maybe Resource,
	_squareNatWonder  :: Bool,
	_squareTokenMarker :: Maybe TokenMarker,
	_squareBuilding :: Maybe Building,
	_squareFigures  :: [Figure]

data TokenMarker =
	ArtifactMarker Artifact |
	HutMarker Hut |
	VillageMarker Village |
	CityMarker City |
	BuildingMarker Building

data BoardTile = BoardTile {
	_boardTileId :: TileID,
	_boardTileCoors :: Coors,
	_boardTileDiscovered :: Bool,
	_boardTileOrientation :: Orientation
-}

tileSquares :: TileID -> [(Coors,Square)]
tileSquares tileid = map ((x,(y,sq)) -> (Coors x y,sq)) $ concatMap (map (zip [0..3])) $ zip [0..3] $ case tileid of
	Tile1 -> [
		[ d c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ mH n_ r_,   g c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   f c_ m_ n_ rI,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   g c_ mH n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ] ]
	Tile2 -> [
		[ w c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ nW r_,   f c_ m_ n_ r_,   w c_ m_ n_ rL,   m c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ mH n_ r_,   g c_ m_ n_ r_ ] ]
	Tile3 -> [
		[ g c_ m_ n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ rL,   m c1 mV n_ r_ ],
		[ g c_ m_ n_ rW,   g c_ mH n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_,   d c_ m_ n_ r_ ] ]
	Tile4 -> [
		[ d c_ m_ n_ rL,   d c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ mC n_ r_,   m c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_ ] ]
	Tile5 -> [
		[ w c_ m_ n_ rW,   d c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ mH n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ mH n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_ ] ]
	Tile6 -> [
		[ d c_ m_ n_ r_,   m c_ mH n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ d c_ m_ n_ rI,   d c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ mH n_ r_,   f c_ m_ n_ r_ ] ]
	Tile7 -> [
		[ f c_ m_ n_ r_,   w c_ m_ nW r_,   f c_ m_ n_ rW,   m c_ mH n_ r_ ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   g c_ mH n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile8 -> [
		[ f c_ m_ n_ rW,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ rL ],
		[ g c_ mV n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ mH n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   f c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_ ] ]
	Tile9 -> [
		[ w c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ rI ],
		[ w c_ m_ n_ r_,   g c_ mS n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile10 -> [
		[ m c_ m_ n_ rR,   m c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ mH n_ r_ ],
		[ d c_ mV n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ nW r_ ],
		[ d c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ] ]
	Tile11 -> [
		[ m c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   f c1 mG n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile12 -> [
		[ g c_ mV n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ rW,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ g c_ mH n_ r_,   m c_ m_ nW r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_ ] ]
	Tile13 -> [
		[ w c_ m_ n_ rL,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ mA n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ m c_ m_ n_ rR,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ] ]
	Tile14 -> [
		[ m c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ mV n_ r_,   g c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ nW r_,   f c_ m_ n_ r_ ],
		[ d c_ m_ n_ rI,   g c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   d c_ mH n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_ ] ]
	Tile15 -> [
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ m c_ m_ n_ rR,   g c_ mH n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_,   d c_ mV n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ rL ] ]
	Tile16 -> [
		[ d c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   g c_ mH n_ r_,   g c_ mH n_ r_,   d c_ m_ n_ rI ],
		[ f c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile17 -> [
		[ d c_ m_ n_ r_,   g c_ m_ n_ r_,   d c_ m_ n_ rI,   g c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   f c_ m_ n_ rW,   g c_ mV n_ r_,   f c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   g c_ mV n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile18 -> [
		[ g c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ mV n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ g c_ m_ n_ rW,   f c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ mH n_ r_ ] ]
	Tile19 -> [
		[ m c_ m_ n_ r_,   m c_ mV n_ r_,   m c_ m_ n_ rR,   d c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   w c1 m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ mV n_ r_ ] ]
	Tile20 -> [
		[ g c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ rW ],
		[ g c_ m_ n_ r_,   g c_ m_ n_ rR,   g c_ mA n_ r_,   g c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile21 -> [
		[ m c_ m_ n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ rR ],
		[ d c_ m_ n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ mH n_ r_ ],
		[ f c_ mH n_ r_,   d c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile22 -> [
		[ m c_ m_ n_ rL,   g c_ mH n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ mH n_ r_,   f c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   g c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ] ]
	Tile23 -> [
		[ m c_ m_ n_ rI,   d c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ mV n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ m c_ mH n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile24 -> [
		[ g c_ mH n_ r_,   w c1 m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ rR ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ rL,   g c_ mH n_ r_ ],
		[ g c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ rW,   d c_ m_ n_ rI ] ]
	Tile25 -> [
		[ d c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   g c_ mV n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ rW ],
		[ m c1 m_ n_ r_,   d c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ mH n_ r_ ] ]
	Tile26 -> [
		[ f c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ rR,   g c_ mH n_ r_ ],
		[ g c_ m_ n_ rW,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ g c_ mV n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ] ]
	Tile27 -> [
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ rL,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ mH n_ r_ ],
		[ w c_ m_ n_ r_,   g c_ mV n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   d c_ m_ n_ r_ ] ]
	Tile America -> [
		[ m c_ m_ n_ rR,   m c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   m c_ m_ n_ rL,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   d c_ m_ n_ r_,   m c_ m_ n_ rI,   f c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ r_,   f c_ m_ n_ rW ] ]
	Tile Arabs -> [
		[ d c_ m_ nW r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ f c_ m_ n_ rW,   m c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ rL ],
		[ f c_ m_ n_ r_,   w c_ m_ n_ rI,   m c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile Aztecs -> [
		[ f c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   d c1 m_ n_ r_,   g c_ m_ n_ rW,   w c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   d c_ m_ n_ r_,   f c_ m_ n_ r_,   d c_ m_ n_ rI ] ]
	Tile China -> [
		[ d c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ m_ n_ rW ],
		[ m c_ m_ n_ rR,   d c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ nW r_,   w c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ rL ] ]
	Tile Egypt -> [
		[ f c_ m_ n_ r_,   g c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ rI ],
		[ d c_ m_ n_ rW,   d c_ m_ n_ r_,   g c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   f c_ m_ n_ r_,   d c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ rL,   m c_ m_ n_ r_ ] ]
	Tile English -> [
		[ w c_ m_ n_ r_,   m c_ m_ n_ rR,   m c_ m_ n_ r_,   w c_ m_ n_ rL ],
		[ w c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ rL,   m c_ m_ n_ rW,   f c_ m_ n_ r_,   g c_ m_ nW r_ ],
		[ g c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ rL ] ]
	Tile French -> [
		[ w c_ m_ n_ r_,   w c_ m_ n_ rL,   f c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ g c_ m_ n_ rW,   g c_ m_ n_ r_,   f c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   g c_ m_ n_ r_,   m c_ m_ n_ rR,   d c_ m_ n_ rI ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile Germany -> [
		[ w c_ m_ n_ rL,   g c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ g c_ m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ rR,   f c_ m_ n_ r_ ],
		[ m c1 m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_,   g c_ m_ n_ rW ] ]
	Tile Greeks -> [
		[ w c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ f c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ rL ],
		[ w c_ m_ n_ r_,   g c_ m_ n_ r_,   d c_ m_ n_ r_,   w c_ m_ n_ rI ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ rW,   w c_ m_ n_ r_,   w c_ m_ nW r_ ] ]
	Tile Indians -> [
		[ m c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ rI,   d c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile Japanese -> [
		[ w c_ m_ n_ rW,   w c_ m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ w c_ m_ n_ rL,   f c_ m_ n_ r_,   m c_ m_ n_ rR,   w c_ m_ n_ r_ ],
		[ g c1 m_ n_ r_,   m c_ m_ n_ r_,   f c_ m_ n_ r_,   w c_ m_ n_ rW ],
		[ m c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile Mongols -> [
		[ m c_ m_ n_ r_,   g c_ m_ nW r_,   f c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   f c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   g c_ m_ n_ rL,   m c_ m_ n_ r_,   d c_ m_ n_ r_ ],
		[ d c_ m_ n_ rL,   d c_ m_ n_ r_,   d c_ m_ n_ rI,   d c_ m_ n_ r_ ] ]
	Tile Rome -> [
		[ m c_ m_ n_ r_,   d c_ m_ n_ rI,   m c_ m_ n_ r_,   f c_ m_ n_ rL ],
		[ w c_ m_ n_ r_,   m c_ m_ n_ r_,   d c_ m_ n_ r_,   m c_ m_ n_ rR ],
		[ f c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ rW,   f c_ m_ n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ r_ ] ]
	Tile Russia -> [
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   m c_ m_ n_ r_,   m c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   m c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ d c_ m_ n_ r_,   f c_ m_ n_ r_,   m c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ f c_ m_ n_ rW,   d c_ m_ n_ r_,   g c_ m_ n_ r_,   m c_ m_ n_ rR ] ]
	Tile Spanish -> [
		[ w c_ m_ n_ r_,   w c_ m_ n_ r_,   g c_ m_ n_ rR,   g c_ m_ n_ r_ ],
		[ w c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ m_ n_ r_,   w c_ m_ n_ r_ ],
		[ w c_ m_ n_ rL,   g c_ m_ n_ rW,   g c_ m_ n_ r_,   w c_ m_ n_ rI ],
		[ w c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ m_ n_ r_,   w c1 m_ n_ r_ ] ]
	TilenZulu -> [
		[ d c_ m_ n_ rI,   m c_ m_ n_ r_,   d c_ m_ n_ rL,   d c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   d c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ d c_ m_ n_ rL,   f c_ m_ n_ rW,   g c_ m_ n_ r_,   g c_ m_ n_ r_ ] ]

	where
	sq terrain coin res natwon tok = Square [terrain] coin res natwon tok Nothing []
	d = sq Desert
	g = sq Grassland
	m = sq Mountains
	f = sq Forest
	w = sq Water
	c_ = False
	c1 = True
	m_ = Nothing
	mH = Just (HutMarker undefined)
	mV = Just (VillageMarker undefined)
	mC = Just (ArtifactMarker ArkOfCovenant)
	mV = Just (ArtifactMarker AttilaVillage)
	mG = Just (ArtifactMarker SevenCitiesOfGold)
	mA = Just (ArtifactMarker Atlantis)
	mS = Just (ArtifactMarker SchoolConfucius)
	n_ = False
	nW = True
	r_ = Nothing
	rI = Just Incense
	rL = Just Linen
	rR = Just Iron
	rW = Just Wheat

data LayoutTile = CT Int Orientation | NT

boardLayout numPlayers = case numPlayers of
	2 -> [
		(c 0  0,CT 0 s), (c 4  0,NT    ),
		(c 0  4,NT    ), (c 4  4,NT    ),
		(c 0  8,NT    ), (c 4  8,NT    ),
		(c 0 12,NT    ), (c 4 12,NT    ),
		(c 0 16,NT    ), (c 4 16,NT    ),
		(c 0 20,NT    ), (c 4 20,NT    ),
		(c 0 24,NT    ), (c 4 24,NT    ),
		(c 0 28,NT    ), (c 4 28,CT 1 n) ]
	n -> error $ "boardLayout for " ++ show n ++ " players not yet implemented!"
	where
	c = Coors
	n = Northward
	s = Southward
	e = Eastward
	w = Westward