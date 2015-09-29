{-# LANGUAGE TypeFamilies #-}

module Actions where

import Prelude

import Model
import Acidic

data ActionAlgebra =
	Atomic Action |
	OneOf [ActionAlgebra] |
	Sequence [ActionAlgebra] |
	Repeat ActionAlgebra
	deriving (Show,Eq,Ord)

--data Phase = StartOfGame | StartOfTurn | Trading | CityManagement | Movement | Research

--data StartOfGame_SubPhase = BuildCity | 

data Action =
	BuildFirstCity | GetFirstTrade |   -- StartOfGame
	GetTrade   -- StartOfTurn
	deriving (Show,Eq,Ord)

phaseActions :: Phase -> ActionAlgebra
phaseActions StartOfGame = Atomic BuildFirstCity
phaseActions _ = OneOf []

{-
data (Ord a) => Value a = SetValue a | ModifyValue (a -> a)

class Card a where
	unitLevel :: UnitType -> Value UnitLevel
	unitLevel _ = SetValue UnitLevelI

	unitStackLimit :: Value Int
	unitStackLimit = SetValue 2

	moveRange :: Value Coor
	moveRange = SetValue 2

	cardCoins :: Value Coins
	cardCoins = SetValue (Coins 0)

	cardActions :: Phase -> [(String,UpdateCivM ())]
	cardActions _ = []
-}


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
