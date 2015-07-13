module Entities where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Map

modelVersion = 0

type UserName = Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    email Text
    password Text Maybe
    verkey Text Maybe
    verified Bool
    UniqueUser email
    deriving Typeable
    deriving Show

Email
    email Text
    user UserId Maybe
    verkey Text Maybe
    UniqueEmail email

|]
