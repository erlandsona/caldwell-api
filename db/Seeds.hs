import Database.Persist.Postgresql
import Data.Time

import Configuration
import Models

accounts =
    [ Account "Austin" "Erlandson" "austin@erlandson.com"
    , Account "Emily" "Kroll" "krollemily@ymail.com"
    ]

gigs =
    [ Gig (UTCTime (fromGregorian 2017 7 21) (secondsToDiffTime 0)) "Belcourt Taps"
    , Gig (UTCTime (fromGregorian 2017 7 22) (secondsToDiffTime 0)) "Bridge Bar"
    ]

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3737
    pool <- makePool env
    let settings = Settings { getPool = pool, getEnv = env }
        logger = setLogger env
    putStrLn $ "Migrating and Seeding DB"
    runSqlPool seed pool

seed :: SqlPersistT IO ()
seed = do
    printMigration migrateAll
    runMigration migrateAll
    mapM_ insert accounts
    mapM_ insert gigs
