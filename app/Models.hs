{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Time (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
    ( mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    User json
        firstName String
        lastName  String
        email     String
        deriving Show

    Venue json
        date UTCTime
        location String
        deriving Show
|]
