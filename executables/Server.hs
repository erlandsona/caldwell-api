{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

-- Libs
import Control.Monad.Reader (runReaderT)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Postgresql (runSqlPool)
import Hilt.Server
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Generic

-- Source
import Config
import Models
import Routes (Router, Routes, ApiRoutes)
import qualified Routes as R

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 3737
    pool <- makePool env

    let settings = Settings { getPool = pool, getEnv = env }
        logger = setLogger env

    runSqlPool doMigrations pool
    putStrLn $ "Serving on PORT: " ++ show port

    let appToHandler :: App a -> Handler a
        appToHandler a = runReaderT (runApp a) settings


    let middlewares :: Middleware
        middlewares = compression
                    . allowCsrf
                    . corsified

    let apiServer :: ApiRoutes AsServer
        apiServer = R.ApiRoutes
            { R.allAccounts = appToHandler (runDb $ selectList [] [] :: App [Entity Account])
            , R.allShows = appToHandler (runDb $ selectList [] [] :: App [Entity Gig])
            , R.createShow = appToHandler . (runDb . insertEntity :: Gig -> App (Entity Gig))
            , R.deleteShow = appToHandler . ((NoContent <$) . runDb . delete :: Key Gig -> App NoContent)
            }

    let server :: Routes AsServer
        server = R.Routes { R.api = toServant apiServer }

    let app :: Application
        app = middlewares
            . serve (Proxy :: Proxy Router)
            $ toServant server

    run port . logger $ app

doMigrations :: SqlPersistT IO ()
doMigrations = do
    printMigration migrateAll
    runMigration migrateAll
