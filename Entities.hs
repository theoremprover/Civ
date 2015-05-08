module Entities where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Map

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    email Text
    password Text Maybe
    verkey Text Maybe
    verified Bool
    UniqueUser email
    participations (Map Text [Text])
    deriving Show

Email
    email Text
    user UserId Maybe
    verkey Text Maybe
    UniqueEmail email

|]
