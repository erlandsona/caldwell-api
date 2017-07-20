{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Libs
import Control.Monad.Except
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Database.Persist.Sql
import Database.Persist.Postgresql (runSqlPool)
import Data.Aeson
import Data.Aeson.TH
import Data.Time
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
import Server.Config
     ( App(..)
     , Config(..)
     , Environment(..)
     , makePool
     , setLogger
     )
import Lib
import Models





app :: Config -> Application
app cfg = corsWithContentType $
    serve (Proxy :: Proxy Api) $ (appToServer cfg)
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

appToServer :: Config -> Server Api
appToServer cfg = enter (convertApp cfg) server

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

server :: ServerT Api App
server = (allUsers :<|> allVenues) :<|> files

allUsers :: App [Entity User]
allUsers = runDb (selectList [] [])

allVenues :: App [Entity Venue]
allVenues = runDb (selectList [] [])

files :: Application
files = serveDirectory "assets"









main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3737
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
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

