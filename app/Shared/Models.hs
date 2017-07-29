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

import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
import Elm
import Elm.Export.Persist.Entity ()
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  firstName Text
  lastName  Text
  email     Text
  deriving Show Generic
  UniqueEmail email

Venue json
    date UTCTime
    location Text
    deriving Show Generic
|]
instance ElmType User
instance ElmType Venue

-- use GeneralizedNewtypeDeriving for ids
-- this picks a simpler int-encoding
-- deriving instance ElmType UserId
-- deriving instance ElmType VenueId
