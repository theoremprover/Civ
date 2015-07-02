module Polls where

import Data.Aeson

import Control.Concurrent.MVar
import Prelude

import Model

data Affected = GameAdmin | GameGame GameName
	deriving (Eq,Show)

data Notification = Notification
	deriving Show

type Polls = MVar [(Affected,MVar Notification)]
