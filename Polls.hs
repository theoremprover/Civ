module Polls where

import Data.Aeson.TH

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
	GameActionA ActionSource Action
	deriving Show

data Affected = GameAdmin | GameGame GameName
	deriving (Eq,Show)

type Polls = MVar [(Affected,MVar ActionA)]


data Action =
	BuildFirstCity GameName PlayerName Coors |
	GetFirstTrade GameName PlayerName
	deriving (Show,Eq,Ord)

coors2action :: Coors -> Action -> Bool
coors2action coors action@(BuildFirstCity _ _ cs) | coors==cs = True
coors2action _ _ = False

data ActionSource =
	CitySource | MetropolisSource |
	FlagSource | WagonSource |
	ResourceSource
	deriving (Show,Eq,Ord)

deriveJSON defaultOptions ''ActionSource
deriveJSON defaultOptions ''ActionA
deriveJSON defaultOptions ''Affected
deriveJSON defaultOptions ''Action
