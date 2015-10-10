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
	WagonSource | FlagSource |
	ResourceSource Resource |
	CitySource | MetropolisSource |
	SquareSource Coors |
	DialCoinSource | DialCultureSource |
	HutSource Hut | VillageSource Village |
	TechCoinSource Tech |
	ArtifactSource Artifact
	deriving (Show,Eq,Ord,Data,Typeable)

data ActionTarget =
	SquareTarget Coors |
	BuildFirstCityTarget Coors |
	TechTarget Tech
	deriving (Show,Eq,Ord,Data,Typeable)

coors2action :: Coors -> ActionTarget -> Bool
coors2action coors (BuildFirstCityTarget cs) | coors==cs = True
coors2action coors (SquareTarget cs) | coors==cs = True
coors2action _ _ = False

deriveJSON defaultOptions ''ActionSource
deriveJSON defaultOptions ''ActionA
deriveJSON defaultOptions ''Affected
deriveJSON defaultOptions ''ActionTarget
deriveJSON defaultOptions ''Figure
deriveJSON defaultOptions ''Resource
deriveJSON defaultOptions ''Tech
deriveJSON defaultOptions ''Hut
deriveJSON defaultOptions ''Village
deriveJSON defaultOptions ''Artifact
