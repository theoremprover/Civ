module Polls where

import Data.Aeson.TH

import Control.Concurrent.MVar
import Prelude

import Model

data Action =
	CreateGameA GameName |
	DeleteGameA GameName |
	JoinGameA GameName PlayerName PlayerEmail Colour Civ |
	IncTradeA GameName PlayerName Trade |
	StartGameA GameName |
	SetSessionGameA GameName |
	SetSessionGamePlayerA GameName PlayerName
	deriving Show

data Affected = GameAdmin | GameGame GameName
	deriving (Eq,Show)

type Polls = MVar [(Affected,MVar Action)]

deriveJSON defaultOptions ''Action
deriveJSON defaultOptions ''Affected

