{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Models where

import Data.Aeson
import Data.Text
import Data.Time (UTCTime)
import Database.Persist.TH
import Elm
import GHC.Generics

-- import Elm.Export.Persist
import Elm.Export.Persist.BackendKey ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  firstName Text
  lastName  Text
  email     Text
  deriving Show Generic
  UniqueEmail email

Venue
    date UTCTime
    location Text
    deriving Show Generic
|]

instance ToJSON   User
instance FromJSON User
instance ElmType  User

instance ToJSON   Venue
instance FromJSON Venue
instance ElmType  Venue

-- use GeneralizedNewtypeDeriving for ids
-- this picks a simpler int-encoding
deriving instance ElmType UserId
deriving instance ElmType VenueId



-- {-# LANGUAGE DeriveGeneric              #-}
-- {-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE GADTs                      #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE QuasiQuotes                #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeFamilies               #-}

-- module Models where

-- import Data.Aeson
-- import Data.Text
-- import Data.Time (UTCTime)
-- -- import Database.Persist.Sql
-- import Database.Persist.TH
--     -- ( mkMigrate
--     -- , mkPersist
--     -- , persistLowerCase
--     -- , share
--     -- , sqlSettings
--     -- )
-- import Elm
-- import Elm.Export.Persist
-- import Elm.Export.Persist.BackendKey ()
-- import GHC.Generics


-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User
--     firstName Text
--     lastName  Text
--     email     Text
--     deriving Show Generic
--     UniqueEmail email

-- Venue
--     date UTCTime
--     location Text
--     deriving Show Generic
-- |]

-- instance ElmType  User
-- instance ToJSON   User
-- instance FromJSON User

-- instance ElmType  Venue
-- instance ToJSON   Venue
-- instance FromJSON Venue

-- deriving instance ElmType UserId
-- deriving instance ElmType VenueId
