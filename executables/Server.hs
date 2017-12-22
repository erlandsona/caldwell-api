{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- Libs
import Control.Monad.Reader (runReaderT)
import Database.Persist.Sql
import Database.Persist.Postgresql (runSqlPool)
import Hilt.Server
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Generic
-- import WaiAppStatic.Types

-- Source
import Config
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

    let convertApp :: App :~> Handler
        convertApp = NT (flip runReaderT settings . runApp)

    let apiServer :: ApiRoutes AsServer
        apiServer = ApiRoutes
            { accounts = enter convertApp allAccounts
            , gigs = enter convertApp allGigs
            }

    let server :: Routes AsServer
        server = Routes { api = toServant apiServer }

    let middlewares :: Middleware
        middlewares = compression
                    . allowCsrf
                    . corsified

    let app :: Application
        app = middlewares
            . serve (Proxy :: Proxy Router)
            $ toServant server

    run port . logger $ app


allAccounts :: App [Account]
allAccounts = do
    dbAccounts <- runDb $ selectList [] []
    return $ entityVal <$> dbAccounts

allGigs :: App [Gig]
allGigs = do
    dbGigs <- runDb $ selectList [] []
    return $ entityVal <$> dbGigs

doMigrations :: SqlPersistT IO ()
doMigrations = do
    printMigration migrateAll
    runMigration migrateAll
