{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
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

-- Source
import Configuration
import Models
import Routes

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3737
    pool <- makePool env
    let settings = Settings { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    putStrLn $ "Serving on PORT: " ++ show port

    let convertApp :: App :~> ExceptT ServantErr IO
        convertApp = Nat (flip runReaderT settings . runApp)

    let apiServer :: ApiRoutes AsServer
        apiServer = ApiRoutes
            { accounts = enter convertApp allAccounts
            , gigs = enter convertApp allGigs
            }

    let server :: Routes AsServer
        server = Routes
            { api = toServant apiServer
            , root = files
            }

    let app :: Application
        app = corsWithContentType $
            serve (Proxy :: Proxy Router) $ toServant server
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

    run port $ logger $ app


allAccounts :: App [Account]
allAccounts = do
    dbAccounts <- runDb $ selectList [] []
    return $ entityVal <$> dbAccounts

allGigs :: App [Gig]
allGigs = do
    dbGigs <- runDb $ selectList [] []
    return $ entityVal <$> dbGigs

files :: Application
files = serveDirectory "static"

doMigrations :: SqlPersistT IO ()
doMigrations = do
    printMigration migrateAll
    runMigration migrateAll
