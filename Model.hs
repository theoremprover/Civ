{-# LANGUAGE
	CPP, DeriveDataTypeable, TypeFamilies, TemplateHaskell, FlexibleContexts, StandaloneDeriving, TypeSynonymInstances,
	GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards, OverloadedStrings, FlexibleInstances #-}

module Model where

import Prelude
import Data.Data
import Data.Maybe
import Data.Ix
import Data.Text (Text(..))
import Data.Typeable
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.List
import Data.Ord
import Data.Monoid
import Data.Time
import Control.Lens
import qualified Data.Map as Map
import Data.Array.IArray
import Data.Aeson.TH

import Entities
import TokenStack
import ModelVersion
import AssocList

import qualified Data.Ix as Ix

allOfThem :: (Ix t,Bounded t) => [t]
allOfThem = range (minBound,maxBound)

type Turn = Int

type Coor = Int
data Coors = Coors { xCoor :: Coor, yCoor :: Coor }
	deriving (Data,Typeable,Eq,Ord,Ix)
instance Show Coors where
	show (Coors x y) = "(" ++ show x ++ "," ++ show y ++")"
$(deriveSafeCopy modelVersion 'base ''Coors)
deriveJSON defaultOptions ''Coors

infix 6 +/+
(Coors x1 y1) +/+ (Coors x2 y2) = Coors (x1+x2) (y1+y2)


newtype Trade = Trade {tradeTrade::Int} deriving (Show,Num,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''Trade)
newtype Coins = Coins {coinsCoins::Int} deriving (Show,Num,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''Coins)
newtype Culture = Culture {cultureCulture::Int} deriving (Show,Num,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''Culture)
newtype Hammers = Hammers {hammersHammers::Int} deriving (Show,Num,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''Hammers)
newtype MilitaryBonus = MilitaryBonus {militaryBonusMilitaryBonus::Int} deriving (Show,Num,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''MilitaryBonus)

addCultureDial (Culture c1) (Culture c2) = Culture $ max (c1+c2) 0
addTradeDial (Trade t1) (Trade t2) = Trade $ max (min (t1+t2) 27) 0

newtype PlayerName = PlayerName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy modelVersion 'base ''PlayerName)

playerName (PlayerName pn) = pn

data Orientation = Northward | Eastward | Southward | Westward
	deriving (Show,Eq,Ord,Ix.Ix,Bounded,Data,Typeable,Enum)
$(deriveSafeCopy modelVersion 'base ''Orientation)

data Colour = Red | Green | Blue | Violet | Yellow | White | Grey
	deriving (Show,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''Colour)

data Civ =
	America | Arabs | Aztecs | China | Egypt | English | French | Germany |
	Greeks | Indians | Japanese | Mongols | Rome | Russia | Spanish | Zulu
	deriving (Show,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''Civ)

data TileID =
	Tile1 | Tile2 | Tile3 | Tile4 | Tile5 | Tile6 | Tile7 | Tile8 | Tile9 | Tile10 |
	Tile11 | Tile12 | Tile13 | Tile14 | Tile15 | Tile16 | Tile17 | Tile18 | Tile19 | Tile20 |
	Tile21 | Tile22 | Tile23 | Tile24 | Tile25 | Tile26 | Tile27 |
	Tile Civ
	deriving (Eq,Show,Data,Ord,Typeable)
$(deriveSafeCopy modelVersion 'base ''TileID)

data Phase = StartOfGame |
	BuildingFirstCity | PlaceFirstFigures | GettingFirstTrade |
	StartOfTurn |
	Trading |
	CityManagement |
	Movement |
	Research |
	Battle
	deriving (Show,Eq,Ord,Enum,Bounded,Ix,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Phase)

allPhases :: [Phase]
allPhases = allOfThem

nextPhase Research = StartOfTurn
nextPhase phase    = succ phase

data Tech =
	Pottery | Writing | CodeOfLaws | Currency | Metalworking | Masonry | Agriculture |
	HorsebackRiding | AnimalHusbandry | Philosophy | Navigation | Navy |
	PublicAdministration | Mysticism | MonarchyTech | DemocracyTech | Chivalry | Mathematics | Logistics |
	PrintingPress | Sailing | Construction | Engineering | Irrigation | Bureaucracy |
	Theology | CommunismTech | Gunpowder | Railroad | MetalCasting | Ecology | Biology |
	SteamEngine | Banking | MilitaryScience | Education |
	Computers | MassMedia | Ballistics | ReplacementParts | Flight | Plastics | CombustionEngine | AtomicTheory |
	SpaceFlight
	deriving (Show,Data,Ord,Ix,Bounded,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Tech)

data TechLevel =
	TechLevelI | TechLevelII | TechLevelIII | TechLevelIV | TechLevelV
	deriving (Show,Eq,Ord,Enum,Data,Ix,Bounded,Typeable)
$(deriveSafeCopy modelVersion 'base ''TechLevel)

techLevelMap :: Map.Map TechLevel [Tech]
techLevelMap = Map.fromList [
	(TechLevelI,   [ Pottery,Writing,CodeOfLaws,Currency,Metalworking,Masonry,HorsebackRiding,
		AnimalHusbandry,Philosophy,Navigation,Navy,Agriculture ]),
	(TechLevelII,  [ PublicAdministration,Mysticism,MonarchyTech,DemocracyTech,Chivalry,
		Mathematics,Logistics,PrintingPress,Sailing,Construction,Engineering,Irrigation,Bureaucracy ]),
	(TechLevelIII, [ Theology,CommunismTech,Gunpowder,Railroad,MetalCasting,Ecology,
		Biology,SteamEngine,Banking,MilitaryScience,Education ]),
	(TechLevelIV,  [ Computers,MassMedia,Ballistics,ReplacementParts,Flight,
		Plastics,CombustionEngine,AtomicTheory ]),
	(TechLevelV,   [ SpaceFlight ]) ]

techsOfLevel level = fromJust $ Map.lookup level techLevelMap
levelOfTech tech = fst $ head $ filter ((elem tech).snd) (Map.assocs techLevelMap)

data Government =
	Anarchy | Despotism | Monarchy | Democracy |
	Fundamentalism | Republic | Feudalism | Communism
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Government)

data Resource = Incense | Wheat | Linen | Iron | Spy | Atom
	deriving (Show,Data,Typeable,Eq,Ord)
$(deriveSafeCopy modelVersion 'base ''Resource)

data ResourcePattern = One { oneResource::Resource } | AnyResource
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''ResourcePattern)

instance Ord ResourcePattern where
	One r1      <= One r2      = r1==r2
	_           <= AnyResource = True
	AnyResource <= _           = True

initialResourceStack :: Int -> TokenStack Resource ()
initialResourceStack numplayers = tokenStackFromList $ replicateUnit [
	(Wheat,numplayers),(Incense,numplayers),(Linen,numplayers),(Iron,numplayers) ]

data Income = Income {
	inTrade    :: Trade,
	inHammers  :: Hammers,
	inCulture  :: Culture,
	inCoins    :: Coins,
	inMilBonus :: MilitaryBonus,
	inResource :: [ResourcePattern] }
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Income)

noIncome = Income (Trade 0) (Hammers 0) (Culture 0) (Coins 0) (MilitaryBonus 0) []

infixl 6 +#
(Income t1 h1 c1 co1 m1 r1) +# (Income t2 h2 c2 co2 m2 r2) = Income (t1+t2) (h1+h2) (c1+c2) (co1+co2) (m1+m2) (r1++r2)

infixl 6 -#
(Income t1 h1 c1 co1 m1 r1) -# (Income t2 h2 c2 co2 m2 r2) = Income (t1-t2) (h1-h2) (c1-c2) (co1-co2) (m1-m2) (r1\\r2)

instance Ord Income where
	(Income t1 h1 c1 co1 m1 r1) <= (Income t2 h2 c2 co2 m2 r2) =
		t1<=t2 && h1<=h2 && c1<=c2 && co1<=co2 && m1<=m2 && all (==True) (zipWith (<=) r1 r2)

instance Monoid Income where
	mempty  = noIncome
	mappend = (+#)

class GeneratesIncome x where
	generatedIncome :: x -> Income

class ConsumesIncome x where
	consumedIncome :: x -> Income

tradeIncome         x = noIncome { inTrade = Trade x }
hammerIncome        x = noIncome { inHammers = Hammers x }
cultureIncome       x = noIncome { inCulture = Culture x }
oneCoin               = noIncome { inCoins = Coins 1 }
militaryBonusIncome x = noIncome { inMilBonus = MilitaryBonus x }
resourceIncome      x = noIncome { inResource = x }

instance ConsumesIncome TechLevel where
	consumedIncome techlevel = tradeIncome $ case techlevel of
		TechLevelI   -> 6
		TechLevelII  -> 11
		TechLevelIII -> 16
		TechLevelIV  -> 21
		TechLevelV   -> 26

data Terrain = Grassland | Desert | Mountains | Forest | Water
	deriving (Show,Data,Typeable,Eq,Bounded,Ix,Ord)
$(deriveSafeCopy modelVersion 'base ''Terrain)

instance GeneratesIncome Terrain where
	generatedIncome terrain = case terrain of
		Grassland -> noIncome
		Desert    -> tradeIncome 1
		Mountains -> hammerIncome 1
		Forest    -> hammerIncome 2
		Water     -> tradeIncome 1

data MovementType = Land | CrossWater | StayInWater | Air
	deriving (Show,Ord,Eq)

movementTypeEndTerrains mt = case mt `elem` [Land,CrossWater] of
	True  -> [Grassland,Desert,Mountains,Forest]
	False -> allOfThem

movementTypeCrossTerrains mt = case mt of
	Land -> [Grassland,Desert,Mountains,Forest]
	_    -> allOfThem

data Artifact = AttilaVillage | Atlantis | ArkOfCovenant | SevenCitiesOfGold | SchoolOfConfucius
	deriving (Show,Data,Ord,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Artifact)

data Hut = ResourceHut Resource | CityStateHut | ThreeCulture | Teacher | FriendlyBarbarians
	deriving (Show,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''Hut)

initialHutStack :: TokenStack () Hut
initialHutStack = tokenStackFromList $ replicateToken [
	(ResourceHut Spy,6),(ResourceHut Wheat,7),(ResourceHut Incense,6),
	(ResourceHut Linen,6),(ResourceHut Iron,3),(ResourceHut Atom,1),
	(CityStateHut,2),(Teacher,1),(ThreeCulture,1),
	(FriendlyBarbarians,2) ]

data Village = ResourceVillage Resource | FourHammers | SixCulture | CityStateVillage |
	CoinVillage | GreatPersonVillage
	deriving (Show,Data,Typeable,Ord,Eq)
$(deriveSafeCopy modelVersion 'base ''Village)

initialVillageStack :: TokenStack () Village
initialVillageStack = tokenStackFromList $ replicateToken [
	(ResourceVillage Spy,4),(ResourceVillage Atom,4),(ResourceVillage Iron,3),
	(CityStateVillage,3),(SixCulture,1),
	(FourHammers,1),(CoinVillage,2),(GreatPersonVillage,2) ]

initialCityStack :: TokenStack () ()
initialCityStack = tokenStackFromList $ replicateUnit [ ((),2) ]

data Walls = NoWalls | Walls
	deriving (Show,Data,Typeable,Eq,Bounded,Ix,Ord,Enum)
$(deriveSafeCopy modelVersion 'base ''Walls)

data City = SecondCitySquare Orientation | City {
	_cityOwner                 :: PlayerName,
	_cityCapital               :: Bool,
	_cityDoubleProd            :: Bool,
	_cityFortified             :: Bool,
	_cityWalls                 :: Walls,
	_cityCaravan               :: Bool,
	_cityIncomeBonus           :: Income,   -- Temporary Bonus per phase, will be reset before CityManagement!
	_cityMetropolisOrientation :: Maybe Orientation
	}
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''City)
makeLenses ''City

instance GeneratesIncome City where
	generatedIncome City{..} =
		cultureIncome (maybe 1 (const 2) _cityMetropolisOrientation) +#
		_cityIncomeBonus

newCity owner capital metroori = City owner capital False False NoWalls False noIncome metroori

data BuildingMarker = BarracksOrAcademy | ForgeOrIronMine |
	GranaryOrAquaeduct | TempleOrCathedral | LibraryOrUniversity |
	MarketOrBank | Harbours | TradePosts | Shipyards
	deriving (Show,Ord,Ix,Enum,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''BuildingMarker)

buildingMarkerToType bm = fromJust $ lookup bm buildingMarkerType
buildingMarkerType = [
	(BarracksOrAcademy,  [Barracks,Academy]),
	(ForgeOrIronMine,    [Forge,IronMine]),
	(GranaryOrAquaeduct, [Granary,Aquaeduct]),
	(TempleOrCathedral,  [Temple,Cathedral]),
	(LibraryOrUniversity,[Library,University]),
	(MarketOrBank,       [Market,Bank]),
	(Harbours,           [Harbour]),
	(TradePosts,         [TradePost]),
	(Shipyards,          [Shipyard]) ]

buildingTypeToMarker :: BuildingType -> BuildingMarker
buildingTypeToMarker bt | bt `elem` [Barracks,Academy]   = BarracksOrAcademy
buildingTypeToMarker bt | bt `elem` [Forge,IronMine]     = ForgeOrIronMine
buildingTypeToMarker bt | bt `elem` [Granary,Aquaeduct]  = GranaryOrAquaeduct
buildingTypeToMarker bt | bt `elem` [Temple,Cathedral]   = TempleOrCathedral
buildingTypeToMarker bt | bt `elem` [Library,University] = LibraryOrUniversity
buildingTypeToMarker bt | bt `elem` [Market,Bank]        = MarketOrBank
buildingTypeToMarker bt | bt `elem` [Harbour]            = Harbours
buildingTypeToMarker bt | bt `elem` [TradePost]          = TradePosts
buildingTypeToMarker bt | bt `elem` [Shipyard]           = Shipyards

data BuildingType = Barracks | Forge | Granary | Harbour | Library |
	Market | Shipyard | TradePost | Temple |
	Academy | Aquaeduct | Bank | Cathedral | IronMine | University
	deriving (Show,Data,Ord,Ix,Enum,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''BuildingType)

terrainBuildings terrain = case terrain of
	Water     -> [ Shipyard,Harbour ]
	Grassland -> [ Granary,Aquaeduct,Library,University,Market,Bank,Barracks,Academy,Temple,Cathedral ]
	Desert    -> [ Market,Bank,Barracks,Academy,Temple,Cathedral,TradePost ]
	Mountains -> [ Market,Bank,Barracks,Academy,Temple,Cathedral,Forge,IronMine ]
	Forest    -> [ Market,Bank,Barracks,Academy,Temple,Cathedral ]

instance GeneratesIncome BuildingType where
	generatedIncome buildingtype = case buildingtype of
		Barracks   -> tradeIncome 2 +# militaryBonusIncome 2
		Forge      -> hammerIncome 3
		Granary    -> hammerIncome 1 +# tradeIncome 1
		Harbour    -> hammerIncome 1 +# tradeIncome 2
		Library    -> cultureIncome 1 +# tradeIncome 1
		Market     -> hammerIncome 1 +# tradeIncome 1 +# cultureIncome 1
		Shipyard   -> hammerIncome 2 +# militaryBonusIncome 2
		TradePost  -> cultureIncome 1 +# tradeIncome 2
		Temple     -> cultureIncome 2
		Academy    -> tradeIncome 2 +# militaryBonusIncome 4
		Aquaeduct  -> hammerIncome 2 +# tradeIncome 2
		Bank       -> hammerIncome 1 +# tradeIncome 1 +# cultureIncome 1 +# oneCoin
		Cathedral  -> cultureIncome 3
		IronMine   -> hammerIncome 4
		University -> cultureIncome 2 +# tradeIncome 2

instance ConsumesIncome BuildingType where
	consumedIncome buildingtype = hammerIncome $ case buildingtype of
		Barracks   ->  7
		Forge      ->  7
		Granary    ->  5
		Harbour    ->  7
		Library    ->  5
		Market     ->  7
		Shipyard   ->  5
		TradePost  ->  7
		Temple     ->  7
		Academy    -> 10
		Aquaeduct  ->  8
		Bank       -> 10
		Cathedral  -> 10
		IronMine   -> 10
		University ->  8

data Building = Building BuildingType PlayerName
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Building)

initialBuildingStack :: TokenStack BuildingMarker ()
initialBuildingStack = tokenStackFromList $ replicateUnit [
	(BarracksOrAcademy,5),(ForgeOrIronMine,6),      (GranaryOrAquaeduct,6),
	(TempleOrCathedral,5),(LibraryOrUniversity,6),(MarketOrBank,5),
	(Harbours,10),        (TradePosts,6),         (Shipyards,5) ]

starBuildingType building = building `elem` [Barracks,Academy,Market,Bank,Shipyard,Temple,Cathedral]

data TokenMarker =
	ArtifactMarker Artifact |
	HutMarker Hut |
	VillageMarker Village |
	CityMarker { _cityMarker :: City } |
	BuildingMarker Building
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''TokenMarker)
makeLenses ''TokenMarker

isHut (Just (HutMarker _)) = True
isHut _ = False

isVillage (Just (VillageMarker _)) = True
isVillage _ = False

isCity (Just (CityMarker _)) = True
isCity _ = False

data Wonder =
	Stonehenge | Colossus | HangingGardens | Oracle | GreatWall |
	ChichenItza | Pyramids | GreatLighthouse | StatueZeus |
	AngkorWat | HimejisCastle | TajMahal | PorcelainTower | MachuPichu |
	BrandenburgGate | Louvre | NotreDame | LeonardosWorkshop |
	SydneyOpera | StatueLiberty | PanamaCanal | UnitedNations |
	BigBen | CristoRedentor | Kremlin | Pentagon | Internet
	deriving (Show,Ord,Ix,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Wonder)

data WonderLevel = WonderLevelI | WonderLevelII | WonderLevelIII 
	deriving (Show,Eq,Ord,Enum,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''WonderLevel)

wondersOfLevel level = case level of
	WonderLevelI   -> [ Stonehenge,Colossus,HangingGardens,Oracle,GreatWall,ChichenItza,Pyramids,GreatLighthouse,StatueZeus ]
	WonderLevelII  -> [ AngkorWat,HimejisCastle,TajMahal,PorcelainTower,MachuPichu,BrandenburgGate,Louvre,NotreDame,LeonardosWorkshop ]
	WonderLevelIII -> [ SydneyOpera,StatueLiberty,PanamaCanal,UnitedNations,BigBen,CristoRedentor,Kremlin,Pentagon,Internet ]

wonderLevel wonder | wonder `elem` (wondersOfLevel WonderLevelI)   = WonderLevelI
wonderLevel wonder | wonder `elem` (wondersOfLevel WonderLevelII)  = WonderLevelII
wonderLevel wonder | wonder `elem` (wondersOfLevel WonderLevelIII) = WonderLevelIII

instance GeneratesIncome Wonder where
	generatedIncome wonder = cultureIncome $ case wonderLevel wonder of
		WonderLevelI   -> 1
		WonderLevelII  -> 2
		WonderLevelIII -> 3

data PolicyCard =
	NaturalOrOrganizedReligion |
	PacifismOrMilitaryTradition |
	ExpansionismOrUrbanDevelopment |
	RationalismOrPatronage
	deriving (Show,Ord,Ix,Data,Bounded,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''PolicyCard)

allPolicyCards :: [PolicyCard]
allPolicyCards = allOfThem

data Policy =
	Rationalism | NaturalReligion | MilitaryTradition | UrbanDevelopment |
	Patronage | Pacifism | OrganizedReligion | Expansionism
	deriving (Show,Ord,Ix,Data,Bounded,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Policy)

cardPolicyBijection = [
	(NaturalOrOrganizedReligion,[ NaturalReligion,OrganizedReligion ]),
	(PacifismOrMilitaryTradition,[ Pacifism,MilitaryTradition ]),
	(ExpansionismOrUrbanDevelopment,[ Expansionism,UrbanDevelopment ]),
	(RationalismOrPatronage,[ Rationalism,Patronage ]) ]
policy2Card p = head [ c | (c,ps) <- cardPolicyBijection, p `elem` ps ]
card2Policies c = fromJust $ lookup c cardPolicyBijection

data CityState = CityState1 | CityState2 | CityState3 | CityState4 | CityState5
	deriving (Show,Ord,Ix,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''CityState)

data FigureType = Flag | Wagon
	deriving (Show,Ord,Ix,Data,Typeable,Eq,Bounded)
$(deriveSafeCopy modelVersion 'base ''FigureType)

data Figure = Figure {
	_figureType      :: FigureType,
	_figureCoors     :: Coors,
	_figureRangeLeft :: Coor }
	deriving (Show,Ord,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Figure)
makeLenses ''Figure

type FigureID = Int

instance ConsumesIncome FigureType where
	consumedIncome Flag  = hammerIncome 4
	consumedIncome Wagon = hammerIncome 6

initialFigureStack :: TokenStack FigureType FigureID
initialFigureStack = tokenStackFromList $ [ (Flag,[0..5]), (Wagon,[10..11]) ]

type SquareFigures = [(PlayerName,FigureID)]

data Square =
	OutOfBounds |
	UnrevealedSquare TileID Coors |
	Square {
		_squareTileIDOri   :: Maybe (TileID,Orientation),
		_squareTerrain     :: [Terrain],
		_squareCoin        :: Bool,
		_squareResource    :: Maybe Resource,
		_squareNatWonder   :: Bool,
		_squareTokenMarker :: Maybe TokenMarker,
		_squareFigures     :: SquareFigures
		}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''Square)
makeLenses ''Square

initialBoardTileStack :: TokenStack () TileID
initialBoardTileStack = tokenStackFromList [((),
	[ Tile1,Tile2,Tile3,Tile4,Tile5,Tile6,Tile7,Tile8,Tile9,Tile10,
		Tile11,Tile12,Tile13,Tile14,Tile15,Tile16,Tile17,Tile18,
		Tile19,Tile20,Tile21,Tile22,Tile23,Tile24,Tile25,Tile26,Tile27 ] )]

data TechCard = TechCard {
	_techCardTechId :: Tech,
	_techCardCoins  :: Coins
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''TechCard)
makeLenses ''TechCard

type PlayerEmail = Text

data Investment =
	EndowmentArts |
	Infrastructure |
	MilitaryIndustrialComplex |
	PublicEducation
	deriving (Show,Data,Ord,Ix,Enum,Bounded,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''Investment)

data GreatPerson = 
	AdaLovelace | AdamSmith | AkiraKurosawa | AlanTuring | AlbertEinstein | AndrewCarnegie |
	APGiannini | Archimedes | CaptainJamesCook | CharlesDarwin | DrMartinLutherKing |
	FlorenceNightingale | FranciscusOfAssisi | FrankLloydWright | FridaKahlo | GalileoGalilei |
	GeorgyZhukov | GustavAdolf | Hannibal | HenryFord | JacquesCousteau | JerryGarcia |
	JimHenson | JoanOfArc | KhalidIbnAlWalid | Leonidas | LorenzoDiMedici | LouisPasteur |
	MarcoPolo | MarieCurie | MarkTwain | Michelangelo | MotherTheresa | NikolaTesla |
	OrvilleWright | SirIsaacNewton | SunTzu | SusanBAnthony | ThomasEdison | Valmiki |
	WilliamShakespeare | ZhengHe
	deriving (Show,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''GreatPerson)
initialGreatPersonStack = tokenStackFromList [ ((),allOfThem) ]

data GreatPersonCard = GreatPersonCard {
	_greatPerson :: GreatPerson,
	_greatPersonCardRevealed :: Bool
	}
	deriving (Show,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''GreatPersonCard)

data UnitBalance = UB_1_3 | UB_2_2 | UB_3_1
	deriving (Show,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''UnitBalance)

data UnitType = Infantry | Cavalry | Artillery | Aircraft
	deriving (Show,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''UnitType)

data UnitCard = UnitCard { unitType::UnitType, unitBalance::UnitBalance }
	deriving (Show,Eq,Data,Typeable,Ord)
$(deriveSafeCopy modelVersion 'base ''UnitCard)

instance ConsumesIncome (UnitType,UnitLevel) where
	consumedIncome (Aircraft,_)      = hammerIncome 12
	consumedIncome (_,UnitLevelI)    = hammerIncome 5
	consumedIncome (_,UnitLevelII)   = hammerIncome 7
	consumedIncome (_,UnitLevelIII)  = hammerIncome 9
	consumedIncome (_,UnitLevelStar) = hammerIncome 11

unit2Ori :: UnitLevel -> Orientation
unit2Ori UnitLevelI    = Northward
unit2Ori UnitLevelII   = Westward
unit2Ori UnitLevelIII  = Southward
unit2Ori UnitLevelStar = Eastward

data UnitLevel = UnitLevelI | UnitLevelII | UnitLevelIII | UnitLevelStar
	deriving (Show,Eq,Data,Typeable,Ix,Bounded,Ord)
$(deriveSafeCopy modelVersion 'base ''UnitLevel)

type Strength = Int
data UnitStrength = UnitStrength { attackStrength::Strength, defenceStrength::Strength }
	deriving (Show,Eq)

initialUnitStack :: TokenStack UnitType UnitCard
initialUnitStack = tokenStackFromList $ map repl [
	(Aircraft, [(UnitCard Aircraft  UB_1_3,2),(UnitCard Aircraft  UB_2_2,4),(UnitCard Aircraft  UB_3_1,2)]),
	(Infantry, [(UnitCard Infantry  UB_1_3,5),(UnitCard Infantry  UB_2_2,5),(UnitCard Infantry  UB_3_1,5)]),
	(Cavalry,  [(UnitCard Cavalry   UB_1_3,5),(UnitCard Cavalry   UB_2_2,5),(UnitCard Cavalry   UB_3_1,5)]),
	(Artillery,[(UnitCard Artillery UB_1_3,5),(UnitCard Artillery UB_2_2,5),(UnitCard Artillery UB_3_1,5)]) ]
	where
	repl (t,l) = (t,concatMap (\ (a,n) -> replicate n a) l)

data CultureEvent =
	Astray | BarbarianEncampment | BankCrisis | BreadCircus | Catastrophe |
	Colonists | Counterfeit | DayOfThePresident | DayOfTheTyrant | Deforestation |
	Disaster | Desertion | Drought | EconomicCrisis | ExchangeIdeas | Faithless |
	Flooding | GenerousGift | GiftFromAfar | HonorAndDuty | Ideas | Immigrants |
	KnightTournament | LongLiveTheQueen | MassDefection | Migrants | Displaced |
	Nationalism | Disoriented | Patriotism | PrimeTime | PrincelyGift | RevoltI | RevoltII |
	RoamingHoarde | Sabotage | SharedKnowledge | SupplyDrop
	deriving (Show,Data,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''CultureEvent)

data CultureLevel = CultureLevelI | CultureLevelII | CultureLevelIII
	deriving (Show,Data,Ord,Bounded,Ix,Typeable,Eq)
$(deriveSafeCopy modelVersion 'base ''CultureLevel)

cultureEventsOfLevel :: CultureLevel -> [CultureEvent]
cultureEventsOfLevel CultureLevelI = [
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
cultureEventsOfLevel CultureLevelII = [
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
cultureEventsOfLevel CultureLevelIII = [
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

cultureEventLevel ev | ev `elem` (cultureEventsOfLevel CultureLevelI) = CultureLevelI
cultureEventLevel ev | ev `elem` (cultureEventsOfLevel CultureLevelII) = CultureLevelII
cultureEventLevel ev | ev `elem` (cultureEventsOfLevel CultureLevelIII) = CultureLevelIII

initialCultureStack :: TokenStack CultureLevel CultureEvent
initialCultureStack = tokenStackFromList [
	(CultureLevelI,  cultureEventsOfLevel CultureLevelI),
	(CultureLevelII, cultureEventsOfLevel CultureLevelII),
	(CultureLevelIII,cultureEventsOfLevel CultureLevelIII) ]	

data CultureStep = DrawCultureCard CultureLevel | DrawGreatPerson
cultureStep step = ([
	Nothing,
	Just $ DrawCultureCard CultureLevelI,
	Just $ DrawCultureCard CultureLevelI,
	Just DrawGreatPerson,
	Just $ DrawCultureCard CultureLevelI,
	Just $ DrawCultureCard CultureLevelI,
	Just $ DrawCultureCard CultureLevelI,
	Just DrawGreatPerson,
	Just $ DrawCultureCard CultureLevelII,
	Just $ DrawCultureCard CultureLevelII,
	Just $ DrawCultureCard CultureLevelII,
	Just $ DrawCultureCard CultureLevelII,
	Just DrawGreatPerson,
	Just $ DrawCultureCard CultureLevelII,
	Just $ DrawCultureCard CultureLevelII,
	Just $ DrawCultureCard CultureLevelIII,
	Just $ DrawCultureCard CultureLevelIII,
	Just $ DrawCultureCard CultureLevelIII,
	Just DrawGreatPerson,
	Just $ DrawCultureCard CultureLevelIII,
	Just $ DrawCultureCard CultureLevelIII,
	Nothing
	] ++ repeat Nothing) !! step

cultureStepCost step | step <= 7 =  (Culture 3,Trade 0)
cultureStepCost step | step <= 14 = (Culture 5,Trade 3)
cultureStepCost step =              (Culture 7,Trade 6)

data CultureCard = CultureCard {
	_cultureCardRevealed :: Bool,
	_cultureCardEvent    :: CultureEvent,
	_cultureCardCoins    :: Coins
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''CultureCard)
makeLenses ''CultureCard

data Production =
	ProduceFigure FigureType |
	ProduceBuilding BuildingType |
	ProduceWonder Wonder |
	ProduceUnit UnitType |
	HarvestResource Resource |
	DevoteToArts Culture
	deriving (Eq,Ord,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Production)
instance Show Production where
	show (ProduceFigure fig) = "Produce " ++ show fig
	show (ProduceBuilding buildingtype) = "Build " ++ show buildingtype
	show (ProduceWonder wonder) = "Build " ++ show wonder
	show (ProduceUnit unittype) = "Produce " ++ show unittype
	show (HarvestResource res) = "Harvest " ++ show res
	show (DevoteToArts (Culture c)) = "Devote to the Arts (" ++ show c ++ " Culture)"

-- The unit types are for ActionSource to be JSON toplevel encodable
data ActionSource =
	AutomaticMove () |
	HaltSource () |
	FigureSource PlayerName FigureType |
	FigureOnBoardSource FigureID PlayerName Coors |
	ResourceSource PlayerName Resource |
	CitySource PlayerName | MetropolisSource PlayerName |
	CityProductionSource Coors Production |
	SquareSource Coors |
	TechSource Tech |
	DialCoinSource PlayerName | DialCultureSource PlayerName |
	HutSource PlayerName Hut | VillageSource PlayerName Village |
	TechCoinSource PlayerName Tech |
	ArtifactSource PlayerName Artifact
	deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''ActionSource)

data ActionTarget =
	NoTarget () |
	DebugTarget String |
	SquareTarget Coors |
	BuildFirstCityTarget PlayerName Coors |
	BuildCityTarget () |
	TechTarget PlayerName Tech |
	TechTreeTarget PlayerName |
	FigureOnBoardTarget FigureID PlayerName Coors |
	GetTradeTarget PlayerName |
	RevealTileTarget Orientation Coors |
	FinishPhaseTarget ()
	deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''ActionTarget)

data Move = Move ActionSource ActionTarget
	deriving (Eq,Ord,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''Move)
instance Show Move where
	show (Move source target) = case (source,target) of
		(_,BuildFirstCityTarget _ coors) -> "Build first city at " ++ show coors
		(FigureOnBoardSource figureid _ sourcecoors,BuildCityTarget ()) -> "Build city at " ++ show sourcecoors ++ " with " ++ show figureid
		(_,GetTradeTarget _) -> "Get Trade"
		(FigureSource _ figure,SquareTarget coors) -> "Place " ++ show figure ++ " on " ++ show coors
		(FigureOnBoardSource figureid _ sourcecoors,SquareTarget targetcoors) -> "Move " ++ show figureid ++ " " ++ show sourcecoors ++ " -> " ++ show targetcoors
		(FigureOnBoardSource figureid _ sourcecoors,RevealTileTarget ori coors) -> "Reveal Tile at " ++ show coors ++ " with " ++ show figureid
		(CityProductionSource _ prod,SquareTarget coors) -> show prod ++ " on " ++ show coors
		(CityProductionSource citycoors prod,NoTarget ()) -> show prod ++ " in " ++ show citycoors
		(TechSource tech,TechTreeTarget _) -> "Research " ++ show tech
		(HaltSource (),_) -> "HALTED"
		(_,DebugTarget msg) -> "DEBUG: " ++ msg
		(_,FinishPhaseTarget ()) -> "Finish Phase"
		(source,target) -> show (source,target)

data Player = Player {
	_playerUserEmail        :: PlayerEmail,
	_playerColour           :: Colour,
	_playerCiv              :: Civ,
	_playerPolicies         :: ([PolicyCard],[Policy]),
	_playerGovernment       :: Government,
	_playerTrade            :: Trade,
	_playerCulture          :: Culture,
	_playerCoins            :: Coins,
	_playerTechs            :: Map.Map TechLevel [TechCard],
	_playerInvestments      :: TokenStack Investment (),
	_playerResources        :: [Resource],
	_playerHuts             :: [Hut],
	_playerVillages         :: [Village],
	_playerArtifacts        :: [Artifact],
	_playerGreatPersonCards :: [GreatPersonCard],
	_playerUnits            :: [UnitCard],
	_playerFigures          :: TokenStack FigureType FigureID,
	_playerFiguresOnBoard   :: Map.Map FigureID Figure,
	_playerCultureCards     :: [CultureCard],
	_playerOrientation      :: Orientation,
	_playerCityStack        :: TokenStack () (),
	_playerCultureSteps     :: Int,
	_playerFirstCityCoors   :: [Coors],
	_playerCityCoors        :: [Coors],
	_playerMoves            :: Map.Map Turn (Map.Map Phase [Move])
	}
	deriving (Data,Typeable,Show)
$(deriveSafeCopy modelVersion 'base ''Player)
makeLenses ''Player

makePlayer useremail colour civ = Player
	useremail colour civ (allPolicyCards,[]) Despotism
	(Trade 0) (Culture 0) (Coins 0) Map.empty
	(tokenStackFromList $ replicateUnit $ map (,0) allOfThem)
	[] [] [] []
	[] [] initialFigureStack Map.empty [] Northward initialCityStack
	0 [] [] Map.empty

data GameState = Waiting | Running | Finished
	deriving (Show,Eq,Ord,Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''GameState)

newtype GameName = GameName Text
	deriving (Data,Typeable,Show,Eq,Ord)
$(deriveSafeCopy modelVersion 'base ''GameName)

gameName (GameName gn) = gn

type Players = AssocList PlayerName Player

emptyPlayers :: Players
emptyPlayers = AssocList []

numPlayers :: Players -> Int
numPlayers = length . fromAssocList

type Board = Array Coors Square

emptyBoard :: Board
emptyBoard = listArray (Coors 1 1,Coors 0 0) []

data Game = Game {
	_gameCreationDate     :: UTCTime,
	_gameCreator          :: UserName,
	_gameState            :: GameState,
	_gamePlayers          :: Players,
	_gameTurn             :: Turn,
	_gamePhase            :: Phase,
	_gameStartPlayer      :: Int,
	_gamePlayersTurn      :: Int,
	_gameBoard            :: Board,
	_gameTileStack        :: TokenStack () TileID,
	_gameHutStack         :: TokenStack () Hut,
	_gameVillageStack     :: TokenStack () Village,
	_gameBuildingStack    :: TokenStack BuildingMarker (),
	_gameGreatPersonStack :: TokenStack () GreatPerson,
	_gameUnitStack        :: TokenStack UnitType UnitCard,
	_gameCultureStack     :: TokenStack CultureLevel CultureEvent,
	_gameResourceStack    :: TokenStack Resource (),
	_gameSpaceFlightTaken :: Bool
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
	_civGames :: Games,
	_civDebugMsg :: String
	}
	deriving (Data,Typeable)
$(deriveSafeCopy modelVersion 'base ''CivState)
makeLenses ''CivState

initialCivState :: CivState
initialCivState = CivState Map.empty ""

data Victory = TechVictory | MilitaryVictory | CultureVictory | EconomicVictory
	deriving (Show,Eq)

civStartTechAndGov civ = case civ of
	America  -> (Currency,       Despotism)
	Arabs    -> (Mathematics,    Despotism)
	Aztecs   -> (Irrigation,     Despotism)
	China    -> (Writing,        Despotism)
	Egypt    -> (Construction,   Despotism)
	English  -> (Navy,           Despotism)
	French   -> (Pottery,        Despotism)
	Germany  -> (Metalworking,   Despotism)
	Greeks   -> (DemocracyTech,  Democracy)
	Indians  -> (Agriculture,    Despotism)
	Japanese -> (Chivalry,       Feudalism)
	Mongols  -> (HorsebackRiding,Despotism)
	Rome     -> (CodeOfLaws,     Republic)
	Russia   -> (CommunismTech,  Communism)
	Spanish  -> (Navigation,     Despotism)
	Zulu     -> (AnimalHusbandry,Despotism)

-------- tileSquare

tileSquares :: TileID -> [(Coors,Square)]
tileSquares tileid = concatMap (\(y,l) -> map (\ (x,sq) -> (Coors x y,sq)) l) $ zip [0..] $ map (zip [0..]) $ case tileid of
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
		[ f c_ m_ n_ r_,   w c_ m_ nW r_,   f c_ m_ n_ rW,   m c_ mV n_ r_ ],
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
		[ g c_ m_ n_ r_,   g c_ m_ n_ rR,   g c_ mT n_ r_,   g c_ m_ n_ r_ ],
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
	Tile Zulu -> [
		[ d c_ m_ n_ rI,   m c_ m_ n_ r_,   d c_ m_ n_ rL,   d c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   g c_ m_ n_ r_,   g c_ m_ n_ r_,   f c_ m_ n_ r_ ],
		[ m c_ m_ n_ r_,   d c_ m_ n_ r_,   f c_ m_ n_ r_,   g c_ m_ n_ r_ ],
		[ d c_ m_ n_ rL,   f c_ m_ n_ rW,   g c_ m_ n_ r_,   g c_ m_ n_ r_ ] ]

	where

	sq terrain coin tok natwon res = Square Nothing [terrain] coin res natwon tok []
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
	mT = Just (ArtifactMarker AttilaVillage)
	mG = Just (ArtifactMarker SevenCitiesOfGold)
	mA = Just (ArtifactMarker Atlantis)
	mS = Just (ArtifactMarker SchoolOfConfucius)
	n_ = False
	nW = True
	r_ = Nothing
	rI = Just Incense
	rL = Just Linen
	rR = Just Iron
	rW = Just Wheat

data LayoutTile = CT Int Orientation | NT

boardLayout :: Int -> [(Coors,LayoutTile)]
boardLayout numplayers = case numplayers of

	2 -> [
		(c 0  0,CT 0 s), (c 4  0,NT    ),
		(c 0  4,NT    ), (c 4  4,NT    ),
		(c 0  8,NT    ), (c 4  8,NT    ),
		(c 0 12,NT    ), (c 4 12,CT 1 n) ]

	3 -> [
		(c 6  0,CT 0 s),
		(c 4  4,NT    ), (c 8  4,NT    ),
		(c 2  8,NT    ), (c 6  8,NT    ), (c 10 8,NT    ),
		(c 0 12,CT 2 e), (c 4 12,NT    ), (c 8 12,NT    ), (c 12 12,CT 1 w) ]

	4 -> [
		(c 0  0,CT 0 s), (c 4  0,NT    ), (c 8  0,NT    ), (c 12  0,CT 1 s),
		(c 0  4,NT    ), (c 4  4,NT    ), (c 8  4,NT    ), (c 12  4,NT    ),
		(c 0  8,NT    ), (c 4  8,NT    ), (c 8  8,NT    ), (c 12  8,NT    ),
		(c 0 12,CT 3 n), (c 4 12,NT    ), (c 8 12,NT    ), (c 12 12,CT 2 n) ]

	5 -> [
		(c 0  2,NT    ), (c 4  0,CT 0 s), (c 8  0,NT    ),
		(c 0  6,NT    ), (c 4  4,NT    ), (c 8  4,NT    ), (c 12  4,CT 1 w),
		(c 0 10,CT 4 e), (c 4  8,NT    ), (c 8  8,NT    ), (c 12  8,NT    ),
		(c 0 14,NT    ), (c 4 12,NT    ), (c 8 12,NT    ), (c 12 12,NT    ),
		(c 0 18,NT    ), (c 4 16,NT    ), (c 8 16,NT    ), (c 12 16,CT 2 w),
		(c 4 20,CT 3 n), (c 8 20,NT    ) ]

	6 -> [
		(c 0  0,NT    ), (c 4  0,CT 0 s), (c  8  0,NT    ),
		(c 0  4,NT    ), (c 4  4,NT    ), (c  8  4,NT    ), (c 12  4,CT 1 w),
		(c 0  8,CT 5 e), (c 4  8,NT    ), (c  8  8,NT    ), (c 12  8,NT    ),
		(c 2 12,NT    ), (c 6 12,NT    ), (c 10 12,NT    ), (c 14 12,NT    ),
		(c 0 16,NT    ), (c 4 16,NT    ), (c  8 16,NT    ), (c 12 16,CT 2 w),
		(c 0 20,CT 4 e), (c 4 20,NT    ), (c  8 20,NT    ), (c 12 20,NT    ),
		                 (c 4 24,NT    ), (c  8 24,CT 3 n), (c 12 24,NT    ) ]

	n -> error $ "boardLayout for " ++ show n ++ " players not yet implemented!"

	where
	c = Coors
	n = Northward
	s = Southward
	e = Eastward
	w = Westward


deriveJSON defaultOptions ''Trade
deriveJSON defaultOptions ''Orientation
deriveJSON defaultOptions ''GameName
deriveJSON defaultOptions ''PlayerName
deriveJSON defaultOptions ''Culture
deriveJSON defaultOptions ''Civ
deriveJSON defaultOptions ''Colour
deriveJSON defaultOptions ''BuildingType
deriveJSON defaultOptions ''Wonder
deriveJSON defaultOptions ''UnitType
deriveJSON defaultOptions ''Production
deriveJSON defaultOptions ''ActionSource
deriveJSON defaultOptions ''ActionTarget
deriveJSON defaultOptions ''Move
deriveJSON defaultOptions ''Tech
deriveJSON defaultOptions ''Artifact
deriveJSON defaultOptions ''FigureType
deriveJSON defaultOptions ''Figure
deriveJSON defaultOptions ''Resource
deriveJSON defaultOptions ''Hut
deriveJSON defaultOptions ''Village
