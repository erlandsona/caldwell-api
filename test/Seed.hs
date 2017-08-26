{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

-- import Import
import Model
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (pgConnStr, withPostgresqlConn, runSqlConn, rawExecute)

accounts =
    [ Account "Austin Erlandson" (Just "password")
    , Account "Emily Kroll" (Just "password")
    ]

emails =
    [ Email "austin@erlandson.com"
    , Email "krollemily@ymail.com"
    ]

main :: IO ()
main = do
    settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
    let conn = (pgConnStr $ appDatabaseConf settings)
    runStderrLoggingT . withPostgresqlConn conn $ runSqlConn $ do
        runMigration migrateAll
        keys <- mapM insert accounts
        mapM_ insert_ $ zipWith (($)) (zipWith (($)) emails keys) [Nothing, Nothing]
