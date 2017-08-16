{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where


import Data.Text
import Data.Time (UTCTime)
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

-- DB Models
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Account json
    firstName Text
    lastName Text
    email Text
    UniqueEmail email
    -- friends [Account]
    deriving Show Generic

Venue json
    date UTCTime
    location Text
    deriving Show Generic
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
    printMigration migrateAll
    runMigration migrateAll


