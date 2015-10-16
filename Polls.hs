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
	GameActionA Move
	deriving Show

data Affected = GameAdmin | GameGame GameName
	deriving (Eq,Show)

type Polls = MVar [(Affected,MVar ActionA)]

-- The unit types are to be JSON toplevel encodable
data ActionSource =
	AutomaticMove () |
	WagonSource PlayerName | FlagSource PlayerName |
	ResourceSource PlayerName Resource |
	CitySource PlayerName | MetropolisSource PlayerName |
	SquareSource Coors |
	DialCoinSource PlayerName | DialCultureSource PlayerName |
	HutSource PlayerName Hut | VillageSource PlayerName Village |
	TechCoinSource PlayerName Tech |
	ArtifactSource PlayerName Artifact |
	HaltSource ()
	deriving (Show,Eq,Ord,Data,Typeable)

data ActionTarget =
	NoTarget () |
	SquareTarget Coors |
	BuildFirstCityTarget PlayerName Coors |
	TechTarget PlayerName Tech |
	GetTradeTarget PlayerName
	deriving (Show,Eq,Ord,Data,Typeable)

data Move = Move ActionSource ActionTarget
	deriving (Eq,Ord,Data,Typeable)
instance Show Move where
	show (Move source target) = case (source,target) of
		(_,BuildFirstCityTarget _ coors) -> "Build first city at " ++ show coors
		(_,GetTradeTarget _) -> "Get Trade"
		(HaltSource (),_) -> "HALTED"
		(source,target) -> show (source,target)

coors2action :: Coors -> Move -> Bool
coors2action coors (Move _ (BuildFirstCityTarget _ cs)) | coors==cs = True
coors2action coors (Move _ (SquareTarget cs)) | coors==cs = True
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
