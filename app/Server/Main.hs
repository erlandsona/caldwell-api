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
import System.Environment (lookupEnv)
import Safe (readMay)

-- Source
import Lib
import Models
import Server.Configuration





app :: Settings -> Application
app cfg = corsWithContentType $
    serve (Proxy :: Proxy Api) $ (appToServer cfg :<|> files)
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

appToServer :: Settings -> Server Endpoints
appToServer cfg = enter (convertApp cfg) server

convertApp :: Settings -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

server :: ServerT Endpoints App
server = allUsers :<|> allVenues

allUsers :: App [Entity User]
allUsers = runDb (selectList [] [])

allVenues :: App [Entity Venue]
allVenues = runDb (selectList [] [])

files :: Application
files = serveDirectory "public"



main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3737
    pool <- makePool env
    let cfg = Settings { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg

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

