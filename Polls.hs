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

coors2action :: Coors -> Move -> Bool
coors2action coors (Move _ (BuildFirstCityTarget _ cs)) | coors==cs = True
coors2action coors (Move _ (SquareTarget cs)) | coors==cs = True
coors2action _ _ = False

deriveJSON defaultOptions ''ActionA
deriveJSON defaultOptions ''Affected
deriveJSON defaultOptions ''Figure
deriveJSON defaultOptions ''Resource
deriveJSON defaultOptions ''Tech
deriveJSON defaultOptions ''Hut
deriveJSON defaultOptions ''Village
deriveJSON defaultOptions ''Artifact
