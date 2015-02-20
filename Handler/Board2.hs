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

data Resource = Incense | Iron | Cloth | Wheat
	deriving (Show,Read,Eq,Ord)
derivePersistField "Resource"

-- TODO: Huts/Villages Ergänzen
data Hut = IncenseHut | WheatHut | IronHut | ClothHut | ThreeCultureHut
	deriving (Show,Read,Eq,Ord)
derivePersistField "Hut"
allHuts = [
	IncenseHut,
	WheatHut, WheatHut,
	IronHut, IronHut, IronHut,
	ClothHut,
	ThreeCultureHut ]

data Village = FriendlyBarbarianVillage | TeacherVillage | WealthVillage | CityStateVillage |
	SpyVillage | AtomVillage
	deriving (Show,Read,Eq,Ord)
derivePersistField "Village"
allVillages = [
	FriendlyBarbarianVillage, TeacherVillage, WealthVillage, CityStateVillage, SpyVillage,
	AtomVillage ]

data Colour = Red | Green | Blue | Violet | Yellow
	deriving (Show,Read,Eq)
derivePersistField "Colour"

data Phase = StartOfTurn | Trade | CityManagement | Movement | Research
	deriving (Show,Read,Eq,Ord,Enum,Bounded)
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

data UnitCard =
	Infantry_1_3 | Infantry_2_2 | Infantry_3_1 |
	Artillery_1_3 | Artillery_2_2 | Artillery_3_1 |
	Cavalry_1_3 | Cavalry_2_2 | Cavalry_3_1 |
	Aircraft_5_7 | Aircraft_6_6 | Aircraft_7_5
	deriving (Show,Read,Eq)
derivePersistField "UnitCard"

unitData :: UnitCard -> (Int,Int,[UnitCard])
unitData unittype = case unittype of
	Infantry_1_3  -> (1,3,cavalryUnits)
	Infantry_2_2  -> (2,2,cavalryUnits)
	Infantry_3_1  -> (3,1,cavalryUnits)
	Artillery_1_3 -> (1,3,infantryUnits)
	Artillery_2_2 -> (2,2,infantryUnits)
	Artillery_3_1 -> (3,1,infantryUnits)
	Cavalry_1_3   -> (1,3,artilleryUnits)
	Cavalry_2_2   -> (2,2,artilleryUnits)
	Cavalry_3_1   -> (3,1,artilleryUnits)
	Aircraft_5_7  -> (5,7,[])
	Aircraft_6_6  -> (6,6,[])
	Aircraft_7_5  -> (7,5,[])

infantryUnits  = [ Infantry_1_3, Infantry_2_2, Infantry_3_1  ]
cavalryUnits   = [ Cavalry_1_3,  Cavalry_2_2,  Cavalry_3_1   ]
artilleryUnits = [ Artillery_1_3,Artillery_2_2,Artillery_3_1 ]

isInfantry  = (`elem` infantryUnits)
isCavalry   = (`elem` cavalryUnits)
isArtillery = (`elem` artilleryUnits)

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

data CultureEvent =
	Astray | BarbarianEncampment | BankCrisis | BreadCircus | Catastrophe |
	Colonists | Counterfeit | DayOfThePresident | DayOfTheTyrant | Deforestation |
	Disaster | Desertion | Drought | EconomicCrisis | ExchangeIdeas | Faithless |
	Flooding | GenerousGift | GiftFromAfar | HonorAndDuty | Ideas | Immigrants |
	KnightTournament | LongLiveTheQueen | MassDefection | Migrants | Displaced |
	Nationalism | Disoriented | Patriotism | PrimeTime | PrincelyGift | RevoltI | RevoltII |
	RoamingHoarde | Sabotage | SharedKnowledge | SupplyDrop
	deriving (Show,Read,Eq)
derivePersistField "CultureEvent"

cultureEventsLevel :: Int -> [CultureEvent]
cultureEventsLevel 1 = [
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
cultureEventsLevel 2 = [
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
cultureEventsLevel 3 = [
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

data GreatPerson =
	AdaLovelace | AdamSmith | AkiraKurosawa | AlanTuring | AlbertEinstein | AndrewCarnegie |
	APGianni | Archimedes | CaptainJamesCook | CharlesDarwin | DrMartinLutherKing |
	FlorenceNightingale | FranciscusOfAssisi | FrankLloydWright | FridaKahlo | GalieoGalilei |
	GeorgyZhukov | GustavAdolf | Hannibal | HenryFord | JacquesCousteau | JerryGarcia |
	JimHenson | JoanOfArc | KhalidIbnAlWalid | Leonidas | LorenzoDiMedici | LouisPasteur |
	MarcoPolo | MarieCurie | MarkTwain | Michelangelo | MotherTheresa | NikolaTesla |
	OrvilleWright | SirIsaacNewton | SunTzu | SusanBAnthony | ThomasEdison | Valmiki |
	WilliamShakespeare | ZhengHe
	deriving (Show,Read,Eq)
derivePersistField "GreatPerson"

greatArtists = [ AkiraKurosawa,FridaKahlo,JerryGarcia,MarkTwain,Valmiki,WilliamShakespeare,Michelangelo ]
greatBuilders = [ AdaLovelace,Archimedes,FrankLloydWright,HenryFord,NikolaTesla,OrvilleWright,ThomasEdison ]
greatGenerals = [ GeorgyZhukov,GustavAdolf,Hannibal,JoanOfArc,KhalidIbnAlWalid,SunTzu,Leonidas ]
greatHumanitarians = [ MotherTheresa,DrMartinLutherKing,JacquesCousteau,FlorenceNightingale,JimHenson,SusanBAnthony,FranciscusOfAssisi ]
greatMerchants = [ APGianni,AdamSmith,AndrewCarnegie,CaptainJamesCook,LorenzoDiMedici,ZhengHe,MarcoPolo ]
greatScientists = [ AlanTuring,AlbertEinstein,CharlesDarwin,GalieoGalilei,SirIsaacNewton,MarieCurie,LouisPasteur ]

data CityType = City | Metropolis
	deriving (Show,Read,Eq)
derivePersistField "CityType"
