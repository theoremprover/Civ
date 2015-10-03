{-# LANGUAGE TypeFamilies,Rank2Types,LambdaCase #-}

module Actions where

import Import(App,getYesod,Handler,MonadHandler,appCivAcid,HandlerSite)
import Prelude

import Control.Lens hiding (Action)
import Data.Acid
import Data.Acid.Advanced

import Model
import Acidic
import Lenses
import Polls

{-
data ActionAlgebra =
	Atomic Action |
	OneOf [ActionAlgebra] |
	Sequence [ActionAlgebra] |
	Repeat ActionAlgebra
	deriving (Show,Eq,Ord)

--data Phase = StartOfGame | StartOfTurn | Trading | CityManagement | Movement | Research

--data StartOfGame_SubPhase = BuildCity | 
-}


queryCivLensH :: (MonadHandler m, HandlerSite m ~ App) => Traversal' CivState a -> m (Maybe a)
queryCivLensH lens = do
	app <- getYesod
	civstate <- query' (appCivAcid app) GetCivState
	return $ preview lens civstate


data Value a = ModifyValue (a -> a) | Unchanged | SetValue a
instance (Ord a) => Ord (Value a) where
	ModifyValue _ <= ModifyValue _ = True
	ModifyValue f <= _ = True
	_ <= ModifyValue g = False
	SetValue a <= SetValue b = a<=b
	SetValue a <= _ = False
	_ <= SetValue b = True
	Unchanged <= Unchanged = True

instance (Show a) => Show (Value a) where
	show (ModifyValue f) = "ModifyValue <fn>"
	show Unchanged = "Unchanged"
	show (SetValue a) = "SetValue " ++ show a
instance (Eq a) => Eq (Value a) where
	ModifyValue _ == _ = False
	_ == ModifyValue _ = False
	Unchanged == Unchanged = True
	SetValue a == SetValue b = a==b
	_ == _ = False

data Abilities = Abilities {
	unitLevel       :: UnitType -> Value UnitLevel,
	unitAttackBonus :: UnitType -> Value Strength,
	unitStackLimit  :: Value Int,
	moveRange       :: Value Coor,
	cardCoins       :: Value Coins,
	maxCities       :: Value Int,
	cardActions     :: Phase -> [(String,PlayerName -> UpdateCivM ())] }

defaultAbilities = Abilities {
	unitLevel       = const $ SetValue UnitLevelI,
	unitAttackBonus = const $ SetValue 0,
	unitStackLimit  = SetValue 2,
	moveRange       = SetValue 2,
	cardCoins       = SetValue (Coins 0),
	maxCities       = SetValue 2,
	cardActions     = const [] }

unchangedAbilities = Abilities {
	unitLevel       = const Unchanged,
	unitAttackBonus = const Unchanged,
	unitStackLimit  = Unchanged,
	moveRange       = Unchanged,
	cardCoins       = Unchanged,
	maxCities       = Unchanged,
	cardActions     = const [] }

civAbilities civ = case civ of
	America  -> defaultAbilities
	Arabs    -> defaultAbilities
	Aztecs   -> defaultAbilities
	China    -> defaultAbilities
	Egypt    -> defaultAbilities
	English  -> defaultAbilities
	French   -> defaultAbilities
	Germany  -> defaultAbilities
	Greeks   -> defaultAbilities
	Indians  -> defaultAbilities
	Japanese -> defaultAbilities {
		unitAttackBonus = \case
			Infantry -> ModifyValue (+1)
			_        -> SetValue 0 }
	Mongols  -> defaultAbilities
	Rome     -> defaultAbilities
	Russia   -> defaultAbilities
	Spanish  -> defaultAbilities {
		moveRange = SetValue 3 }
	Zulu     -> defaultAbilities

techAbilities tech = case tech of
	Pottery              -> unchangedAbilities
	Writing              -> unchangedAbilities
	CodeOfLaws           -> unchangedAbilities
	Currency             -> unchangedAbilities
	Metalworking         -> unchangedAbilities
	Masonry              -> unchangedAbilities
	Agriculture          -> unchangedAbilities
	HorsebackRiding      -> unchangedAbilities
	AnimalHusbandry      -> unchangedAbilities
	Philosophy           -> unchangedAbilities
	Navigation           -> unchangedAbilities
	Navy                 -> unchangedAbilities
	PublicAdministration -> unchangedAbilities
	Mysticism            -> unchangedAbilities
	MonarchyTech         -> unchangedAbilities
	DemocracyTech        -> unchangedAbilities {
		unitLevel = \case
			Infantry -> SetValue UnitLevelII
			_        -> Unchanged }
	Chivalry             -> unchangedAbilities
	Mathematics          -> unchangedAbilities
	Logistics            -> unchangedAbilities
	PrintingPress        -> unchangedAbilities
	Sailing              -> unchangedAbilities
	Construction         -> unchangedAbilities
	Engineering          -> unchangedAbilities
	Irrigation           -> unchangedAbilities
	Bureaucracy          -> unchangedAbilities
	Theology             -> unchangedAbilities
	CommunismTech        -> unchangedAbilities
	Gunpowder            -> unchangedAbilities {
		unitLevel = \case
			Infantry -> SetValue UnitLevelIII
			_        -> Unchanged }
	Railroad             -> unchangedAbilities
	MetalCasting         -> unchangedAbilities
	Ecology              -> unchangedAbilities
	Biology              -> unchangedAbilities
	SteamEngine          -> unchangedAbilities
	Banking              -> unchangedAbilities
	MilitaryScience      -> unchangedAbilities
	Education            -> unchangedAbilities
	Computers            -> unchangedAbilities
	MassMedia            -> unchangedAbilities
	Ballistics           -> unchangedAbilities
	ReplacementParts     -> unchangedAbilities
	Flight               -> unchangedAbilities
	Plastics             -> unchangedAbilities
	CombustionEngine     -> unchangedAbilities
	AtomicTheory         -> unchangedAbilities
	SpaceFlight          -> unchangedAbilities

valueAbilities :: (Ord a) => [Value a] -> a
valueAbilities values = foldl (\ x f -> f x) a modvalues
	where
	SetValue a = maximum values
	modvalues = map ismod values
	ismod (ModifyValue f) = f
	ismod _ = id

unitLevelAbilities :: Player -> UnitType -> UnitLevel
unitLevelAbilities player@(Player{..}) unittype = valueAbilities $
	map (\ f -> f unittype) $ map unitLevel $ civAbilities _playerCiv : map (techAbilities._techCardTechId) _playerTechs

moveGen :: GameName -> Game -> Maybe PlayerName -> [Action]
moveGen _ _ Nothing = []
moveGen gamename game@(Game{..}) (Just my_playername) = do
	let
		(playername_turn,player_turn@(Player{..})) = nthAssocList _gamePlayersTurn _gamePlayers
		in case my_playername == playername_turn of
			False -> []
			True -> case _gamePhase of
				StartOfGame -> []
				BuildingFirstCity -> map (BuildFirstCity gamename playername_turn) _playerFirstCityCoors
				GettingFirstTrade -> [GetFirstTrade gamename playername_turn]
				_ -> []

{-

Arten von Fähigkeiten:
1. Zusätzliche Aktion (z.B. StartOfTurn: Kultureinkommen, Forschung; CityManagement: Zusätzl. Aktion)
2. Modifizierend (Bewegungsweite,Einkommen,Figurenlimit,Mögliche Regierungsform,...)

StartOfTurn:
- Regierungsform ändern
- Persönlichkeit einsetzen/runternehmen

Trading:
- Handel bekommen (abhängig von Wundern, Karten etc.)

CityManagement:
- Für jede Stadt: Produktion | Kultur machen | Resource ernten

Movement:
- Bewegen (bewegungsweite hängt von Techs/Persönlichkeiten ab)

Research:
- Forschung

ALLE:
- Resourcefähigkeit spielen
- Persönlichkeiten spielen
- Kulturkarte spielen

-}
