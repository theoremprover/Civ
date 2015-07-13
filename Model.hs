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
import Data.Array.IArray

import Entities

import qualified Data.Ix as Ix

modelVersion = 0

data Coors = Coors { xCoor :: Int, yCoor :: Int }
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Coors)

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
	deriving (Show,Read,Eq,Data,Typeable)
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
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Tech)

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

data Terrain = Grassland | Desert | Mountains | Forest | Water | Steppe
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Terrain)

data Artifact = AttilaVillage | Atlantis | ArkOfCovenant | SevenCitiesOfGold | SchoolOfConfucius
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Artifact)

data Hut = ResourceHut Resource | CityStateHut | ThreeCulture | TeacherHut | FriendlyBarbarianHut
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Hut)

initialHutSet = concatMap (ResourceHut.(uncurry replicate))
	[ (6,Spy),(7,Wheat),(6,Incense),(6,Linen),(3,Iron),(1,Atom) ] ++
	[ CityStateHut,CityStateHut,TeacherHut,ThreeCulture,
		FriendlyBarbarianHut,FriendlyBarbarianyHut ]

data Village = ResourceVillage Resource | FourHammers | SixCulture | CityStateVillage |
	CoinVillage | GreatPersonVillage
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Village)

initialVillageSet = concatMap (ResourceVillage.(uncurry replicate))
	[ (4,Spy),(4,Atom),(3,Iron) ] ++
	[ CityStateVillage,CityStateVillage,CityStateVillage,SixCulture,
		FourHammers,CoinVillage,CoinVillage,GreatPersonVillage,GreatPersonVillage ]

data City = SecondCitySquare Orientation | City {
	_cityOwner :: PlayerName,
	_cityCapital :: Bool,
	_cityDoubleProd :: Bool,
	_cityFortified :: Bool,
	_cityCaravan :: Bool,
	_cityMetropolis :: Maybe Orientation
	}
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''City)

data BuildingType = BarracksOrAcademy | ForgeOrForge2 |
	GranaryOrAquaeduct | TempleOrCathedral | LibraryOrUniversity |
	MarketOrBank | Harbours | TradeStations | Shipyards
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Building)

data Building = Barracks | Forge | Granary | Harbour | Library |
	Market | Shipyard | TradeStation | Temple |
	Academy | Aquaeduct | Bank | Cathedral | Forge2 | University
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Building)

initialBuildingStacks = Map.fromList $ concatMap (uncurry replicate) [
	(9,BarracksOrAcademy),(9,ForgeOrForge2),(9,GranaryOrAquaeduct),
	(9,TempleOrCathedral),(9,LibraryOrUniversity),(9,MarketOrBank,),
	(9,Harbours),(9,TradeStations),(9,Shipyards) ]

data Plate =
	ArtifactPlate Artifact |
	HutPlate Hut |
	VillagePlate Village |
	CityPlate City |
	BuildingPlate Building
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Plate)

data Figure =
	deriving (Show,Read,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Figure)

data Square = Square {
	_squareTerrain  :: [Terrain],
	_squareCoin     :: Bool,
	_squareResource :: Resource,
	_squareCulture  :: Bool,
	_squarePlate    :: Maybe Plate,
	_squareFigures  :: [Figure]
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''Square)
makeLenses ''Square

data BoardTile = BoardTile {
	_boardTileId :: TileID,
	_boardTileCoors :: Coors,
	_boardTileDiscovered :: Bool,
	_boardTileOrientation :: Orientation,
	_boardTileSquares :: Array 
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

makePlayer useremail colour civ = Player useremail colour civ Anarchy (Trade 0) (Culture 0) (Coins 0) []

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
	_gamePlayers :: Players
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

newGame :: UserName -> UTCTime -> Game
newGame creator utctime = Game utctime creator Waiting [] []

type Games = Map.Map GameName Game

data CivState = CivState {
	_civGames :: Games
	}
	deriving (Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''CivState)
makeLenses ''CivState

initialCivState :: IO CivState
initialCivState = do
	now <- getCurrentTime
	return $ CivState $ Map.fromList [
		(GameName "testgame",Game now "public@thinking-machines.net" Running [
			BoardTile (Tile Russia) (Coors 0 0) True Southward,
			BoardTile Tile1 (Coors 4 0) True Eastward,
			BoardTile Tile2 (Coors 0 4) True Southward,
			BoardTile Tile3 (Coors 4 4) False Southward,
			BoardTile Tile4 (Coors 0 8) False Southward,
			BoardTile Tile5 (Coors 4 8) True Northward,
			BoardTile Tile6 (Coors 0 12) True Westward,
			BoardTile (Tile America) (Coors 4 12) True Northward ]
			[(PlayerName "Spieler Rot", Player "public@thinking-machines.net" Red Russia Despotism (Trade 1) (Culture 6) (Coins 1) [
					TechCard CodeOfLaws TechLevelI (Coins 2),
					TechCard HorsebackRiding TechLevelI (Coins 0),
					TechCard AnimalHusbandry TechLevelI (Coins 0),
					TechCard Philosophy TechLevelI (Coins 0),
					TechCard Navigation TechLevelI (Coins 0),
					TechCard Navy TechLevelI (Coins 0),
					TechCard MonarchyTech TechLevelII (Coins 0) ]),
				(PlayerName "Spieler Blau", Player "reitmeier@thinking-machines.net" Blue America Democracy (Trade 2) (Culture 11) (Coins 3) [
					TechCard CodeOfLaws TechLevelI (Coins 1),
					TechCard HorsebackRiding TechLevelI (Coins 0),
					TechCard AnimalHusbandry TechLevelI (Coins 0),
					TechCard Philosophy TechLevelI (Coins 0),
					TechCard Navigation TechLevelI (Coins 0),
					TechCard Navy TechLevelI (Coins 0),
					TechCard MonarchyTech TechLevelII (Coins 0),
					TechCard PrintingPress TechLevelII (Coins 0),
					TechCard Sailing TechLevelII (Coins 0),
					TechCard Construction TechLevelII (Coins 0),
					TechCard Engineering TechLevelII (Coins 0),
					TechCard SteamEngine TechLevelIII (Coins 0),
					TechCard Banking TechLevelIII (Coins 0),
					TechCard MilitaryScience TechLevelIII (Coins 0),
					TechCard Computers TechLevelIV (Coins 0),
					TechCard MassMedia TechLevelIV (Coins 0),
					TechCard SpaceFlight TechLevelV (Coins 0) ])
				]),

		(GameName "Testgame 2", Game now "public@thinking-machines.net" Waiting []
			[
				(PlayerName "Spieler Blau", Player "public@thinking-machines.net" Blue America Democracy (Trade 0) (Culture 0) (Coins 0) [])
			] )
		]

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
