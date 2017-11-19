{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

-- import Control.Exception (throwIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Control.Monad.Reader
    ( MonadIO
    , MonadReader
    , ReaderT
    , asks
    , liftIO
    )
-- import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.Persist.Postgresql
    ( ConnectionPool
    , ConnectionString
    , SqlPersistT
    , createPostgresqlPool
    , runSqlPool
    )
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Safe (readMay)
import Servant
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
    { runApp :: ReaderT Settings (Handler) a
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
    let keys = ["host=", "dbname=", "user=", "password=", "port="]
        envs =
            case env of
                Production -> ["RDS_HOSTNAME", "RDS_DB_NAME", "RDS_USERNAME", "RDS_PASSWORD", "RDS_PORT"]
                _ -> ["DB_HOST", "DB_NAME", "DB_USERNAME", "DB_PASSWORD", "DB_PORT"]
        defaults =
            [ "localhost"
            , "caldwell" <>
                case env of
                    Production -> ""
                    Development -> "_development"
                    Test -> "_test"
            , "postgres", "", "5432"
            ]
    envVars <- traverse lookupEnv envs

    -- let dbConnection :: Environment -> LoggingT IO ConnectionPool
    let makeConnStr :: ConnectionString
        makeConnStr = BS.pack . intercalate " " $ zip3WithDefaults keys defaults envVars

    case env of
        Production -> do
            -- pool <- runMaybeT $ do
            --     prodStr <- MaybeT . lookupEnv $ "DATABASE_URL"
            --     runStdoutLoggingT $ createPostgresqlPool (BS.pack prodStr) (envPool Production)

            -- case pool of
            --     Just a -> return a
            --     Nothing -> throwIO (userError "Database Configuration not present in environment.")
            return =<< runStdoutLoggingT $ createPostgresqlPool makeConnStr (envPool Production)
        Development ->
            return =<< runStdoutLoggingT $ createPostgresqlPool makeConnStr (envPool Development)

        Test ->
            return =<< runNoLoggingT $ createPostgresqlPool makeConnStr (envPool Test)

zip3WithDefaults :: [String] -> [String] -> [Maybe String] -> [String]
zip3WithDefaults = zipWith3 $ \key def envVar -> key <> fromMaybe def envVar

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool env =
    case env of
        Test -> 1
        Development -> 1
        Production -> 8

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
