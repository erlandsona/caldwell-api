{-# LANGUAGE DeriveGeneric     #-}
module Types where

import Data.Time (UTCTime)
import GHC.Generics (Generic)


data User = User
    { userId        :: Int
    , firstName :: String
    , lastName  :: String
    , email     :: String
    } deriving
        ( Eq
        , Generic
        , Show
        )

data Venue = Venue
    { venueId   :: Int
    , date      :: UTCTime
    , location  :: String
    } deriving
        ( Eq
        , Generic
        , Show
        )
