module Entities where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
    email Text
    password Text Maybe
    verkey Text Maybe
    verified Bool
    UniqueUser email
    participations [(String,[String])]
    deriving Show

Email
    email Text
    user UserId Maybe
    verkey Text Maybe
    UniqueEmail email

|]
