{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Libs
import Control.Monad.Except
import Control.Monad.Reader (runReaderT)
import Database.Persist.Sql
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
    ( cors
    , corsRequestHeaders
    , simpleCorsResourcePolicy
    )
import Servant
import Servant.Generic
import System.Environment (lookupEnv)
import Safe (readMay)

-- Source
import Apis
import Models
import Configuration


app :: Settings -> Application
app settings = corsWithContentType $
    serve (Proxy :: Proxy Root) $ toServant (apiServer settings)
    where
        corsWithContentType :: Middleware
        corsWithContentType = cors (const $ Just policy)
            where
              policy = simpleCorsResourcePolicy
                { corsRequestHeaders =
                    [ "Content-Type"
                    , "Access-Control-Allow-Origin"
                    ]
                }

-- appToServer :: Settings -> Server Root
-- appToServer settings = apiServer settings

convertApp :: Settings -> App :~> ExceptT ServantErr IO
convertApp settings = Nat (flip runReaderT settings . runApp)

apiServer :: Settings -> Endpoints AsServer
apiServer settings = Endpoints
    -- { root = files
    { accounts = enter (convertApp settings) allAccounts
    , venues = enter (convertApp settings) allVenues
    }

allAccounts :: App [Account]
allAccounts = do
    dbAccounts <- runDb $ selectList [] []
    let apiAccounts = map (convertDbAccount . entityVal) dbAccounts
    return apiAccounts

allVenues :: App [Venue]
allVenues = do
    dbVenues <- runDb $ selectList [] []
    let apiVenues = map (convertDbVenue . entityVal) dbVenues
    return apiVenues

files :: Application
files = serveDirectory "public"



main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3737
    pool <- makePool env
    let settings = Settings { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app settings

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

