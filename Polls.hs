module Polls where

import Data.Aeson.TH
import Data.Data

import Control.Concurrent.MVar
import Prelude

import Model

data ActionA =
	CreateGameA GameName |
	DeleteGameA GameName |
	JoinGameA GameName PlayerName PlayerEmail Colour Civ |
	StartGameA GameName |
	SetSessionGameA GameName |
	SetSessionGamePlayerA GameName PlayerName |
	GameActionA [ActionSource] [ActionTarget]
	deriving Show

data Affected = GameAdmin | GameGame GameName
	deriving (Eq,Show)

type Polls = MVar [(Affected,MVar ActionA)]

data ActionSource =
	WagonSource PlayerName | FlagSource PlayerName |
	ResourceSource PlayerName Resource |
	CitySource PlayerName | MetropolisSource PlayerName |
	SquareSource Coors |
	DialCoinSource PlayerName | DialCultureSource PlayerName |
	HutSource PlayerName Hut | VillageSource PlayerName Village |
	TechCoinSource PlayerName Tech |
	ArtifactSource PlayerName Artifact
	deriving (Show,Eq,Ord,Data,Typeable)

data ActionTarget =
	SquareTarget Coors |
	BuildFirstCityTarget PlayerName Coors |
	TechTarget PlayerName Tech
	deriving (Show,Eq,Ord,Data,Typeable)

data Move = Move ActionSource ActionTarget
	deriving (Show,Eq,Ord,Data,Typeable)

coors2action :: Coors -> ActionTarget -> Bool
coors2action coors (BuildFirstCityTarget _ cs) | coors==cs = True
coors2action coors (SquareTarget cs) | coors==cs = True
coors2action _ _ = False

deriveJSON defaultOptions ''ActionSource
deriveJSON defaultOptions ''ActionA
deriveJSON defaultOptions ''Move
deriveJSON defaultOptions ''Affected
deriveJSON defaultOptions ''ActionTarget
deriveJSON defaultOptions ''Figure
deriveJSON defaultOptions ''Resource
deriveJSON defaultOptions ''Tech
deriveJSON defaultOptions ''Hut
deriveJSON defaultOptions ''Village
deriveJSON defaultOptions ''Artifact
