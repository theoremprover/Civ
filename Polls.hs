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
	SetSessionGamePlayerA GameName PlayerName
	deriving Show

data Affected = GameAdmin | GameGame GameName
	deriving (Eq,Show)

type Polls = MVar [(Affected,MVar ActionA)]

deriveJSON defaultOptions ''ActionA
deriveJSON defaultOptions ''Affected
