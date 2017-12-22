import Database.Persist.Postgresql
import Data.Time

import Config
import Models

accounts :: [Account]
accounts =
    [ Account "Austin" "Erlandson" "austin@erlandson.com"
    , Account "Emily" "Kroll" "krollemily@ymail.com"
    ]

gigs :: [Gig]
gigs =
    [ Gig (date "2017-04-15 19:00") "Belcourt Taps"
    , Gig (date "2017-04-26 19:00") "SLOCO"
    , Gig (date "2017-05-03 18:00") "Blue Moon Waterfront Grille"
    , Gig (date "2017-05-08 21:00") "The Commodore"
    , Gig (date "2017-05-28 19:00") "The Commodore"
    , Gig (date "2017-06-07 18:00") "Tavern 96"
    , Gig (date "2017-06-13 15:00") "12th South Farmers Market"
    , Gig (date "2017-06-17 14:00") "Natchez Hills Winery"
    , Gig (date "2017-06-20 21:40") "Bridge Bar"
    , Gig (date "2017-07-11 18:00") "Artist Round @ Commodore Grille"
    , Gig (date "2017-07-25 16:30") "12th South Farmers Market"
    , Gig (date "2017-07-26 15:30") "East Nashville Farmers Market"
    , Gig (date "2017-07-29 09:30") "Richland Park Farmers Market"
    , Gig (date "2017-08-27 14:00") "Natchez Hills Winery"
    , Gig (date "2017-09-05 20:00") "Bridge Bar"
    , Gig (date "2017-09-17 14:00") "Natchez Hills Winery"
    , Gig (date "2017-10-06 21:00") "SomeWhere SOS"
    , Gig (date "2017-10-17 19:00") "SOUTH"
    , Gig (date "2017-11-07 20:30") "Bridge Bar"
    , Gig (date "2017-11-12 19:00") "Belcourt Taps"
    , Gig (date "2017-11-15 00:00") "Belcourt Taps -- Time TBD"
    , Gig (date "2017-11-27 19:00") "Bunganut Pig"
    , Gig (date "2017-11-28 19:45") "Blue Bar"
    , Gig (date "2017-12-01 21:00") "Belcourt Taps"
    ]

date :: String -> UTCTime
date str = zonedTimeToUTC . read $ str ++ ":00 CDT"

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
