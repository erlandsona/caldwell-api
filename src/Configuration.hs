{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader
    ( MonadIO
    , MonadReader
    , ReaderT
    , asks
    , liftIO
    )
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.Persist.Postgresql
    ( ConnectionPool
    , SqlPersistT
    , createPostgresqlPool
    , runSqlPool
    )
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Safe (readMay)
import Servant (ServantErr)
import System.Environment (lookupEnv)

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT Config', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype App a
    = App
    { runApp :: ReaderT Settings (ExceptT ServantErr IO) a
    } deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader Settings
        , MonadError ServantErr
        , MonadIO
        )

-- | The Settings for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Settings
    = Settings
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> IO ConnectionPool
makePool env = do
    let keys = ["host=", "port=", "user=", "password=", "dbname="]
        envs = ["DATABASE_URL", "DB_PORT", "DB_USERNAME", "DB_PASSWORD", "DB_NAME"]
        defaults =
            ["localhost" , "5432", "postgres", ""
            , "caldwell" <> case env of
                Test -> "_test"
                Development -> "_development"
                Production -> "_production"
            ]
    pool <- do
        envVars <- traverse lookupEnv envs
        let dbConnectionString = BS.pack . intercalate " " $ zip3WithDefaults keys defaults envVars
        case env of
            Production ->
                runStdoutLoggingT $ createPostgresqlPool dbConnectionString (envPool Production)

            Development ->
                runStdoutLoggingT $ createPostgresqlPool dbConnectionString (envPool Development)

            Test ->
                runNoLoggingT $ createPostgresqlPool dbConnectionString (envPool Test)

    return pool

zip3WithDefaults :: [String] -> [String] -> [Maybe String] -> [String]
zip3WithDefaults = zipWith3 $ \key def envVar -> key <> fromMaybe def envVar

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

runDb :: (MonadReader Settings m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

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
