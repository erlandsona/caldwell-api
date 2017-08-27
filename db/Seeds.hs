import Database.Persist.Postgresql
import Data.Time

import Configuration
import Models

accounts :: [Account]
accounts =
    [ Account "Austin" "Erlandson" "austin@erlandson.com"
    , Account "Emily" "Kroll" "krollemily@ymail.com"
    ]

gigs :: [Gig]
gigs =
    [ Gig (UTCTime (fromGregorian 2017 7 21) offset) "Belcourt Taps"
    , Gig (UTCTime (fromGregorian 2017 7 22) offset) "Bridge Bar"
    ]

    where
        offset = secondsToDiffTime $ cst * 60 * 60
        cst = 5

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    pool <- makePool env
    putStrLn $ "Migrating and Seeding DB"
    runSqlPool seed pool

seed :: SqlPersistT IO ()
seed = do
    printMigration migrateAll
    runMigration migrateAll
    mapM_ insert accounts
    mapM_ insert gigs
