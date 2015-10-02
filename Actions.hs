{-# LANGUAGE TypeFamilies,Rank2Types #-}

module Actions where

import Import(App,getYesod,Handler,MonadHandler,appCivAcid,HandlerSite)
import Prelude

import Control.Lens hiding (Action)
import Data.Acid
import Data.Acid.Advanced

import Model
import Acidic
import Lenses

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


data Action =
	BuildFirstCity | GetFirstTrade |   -- StartOfGame
	GetTrade   -- StartOfTurn
	deriving (Show,Eq,Ord)

data (Ord a) => Value a = SetValue a | ModifyValue (a -> a)

data Abilities = Abilities {
	unitLevel      :: UnitType -> Value UnitLevel,
	unitStackLimit :: Value Int,
	moveRange      :: Value Coor,
	cardCoins      :: Value Coins,
	maxCities      :: Value Int,
	cardActions    :: Phase -> [(String,PlayerName -> UpdateCivM ())]
	}
defaultAbilities = Abilities {
	unitLevel      = const $ SetValue UnitLevelI,
	unitStackLimit = SetValue 2,
	moveRange      = SetValue 2,
	cardCoins      = SetValue (Coins 0),
	maxCities      = SetValue 2,
	cardActions    = const []
	}

possibleActions :: GameName -> PlayerName -> Handler [Action]
possibleActions gamename playername = do
	Just (game@(Game{..})) <- queryCivLensH $ civGameLens gamename . _Just
	let playername_turn = fst $ nthAssocList _gamePlayersTurn _gamePlayers
	case playername == playername_turn of
		False -> return []
		True -> case _gamePhase of
			StartOfGame -> return []
			BuildingFirstCity -> return [BuildFirstCity]
			GettingFirstTrade -> return [GetFirstTrade]
			_ -> return []

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
