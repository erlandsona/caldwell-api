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


app :: Settings -> Application
app settings = corsWithContentType $
    serve (Proxy :: Proxy Router) $ toServant (server settings)
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

convertApp :: Settings -> App :~> ExceptT ServantErr IO
convertApp settings = Nat (flip runReaderT settings . runApp)

server :: Settings -> Routes AsServer
server settings = Routes
    { api = toServant (apiServer settings)
    , root = files
    }

apiServer :: Settings -> ApiRoutes AsServer
apiServer settings = ApiRoutes
    { accounts = enter (convertApp settings) allAccounts
    , gigs = enter (convertApp settings) allGigs
    }

allAccounts :: App [Account]
allAccounts = do
    dbAccounts <- runDb $ selectList [] []
    return $ entityVal <$> dbAccounts

allGigs :: App [Gig]
allGigs = do
    dbGigs <- runDb $ selectList [] []
    return $ entityVal <$> dbGigs

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
    putStrLn $ "Serving on PORT: " ++ show port
    run port $ logger $ app settings

doMigrations :: SqlPersistT IO ()
doMigrations = do
    printMigration migrateAll
    runMigration migrateAll
