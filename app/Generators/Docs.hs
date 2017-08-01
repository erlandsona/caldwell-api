{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Database.Persist (Entity(..))
import Database.Persist.Sql (toSqlKey)
import Prelude hiding (writeFile)
import Servant.Docs
import Turtle

import Apis
import Models



docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] (Proxy :: Proxy Root)

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]


instance ToSample (Entity User) where
    toSamples _ = singleSample $ Entity key user
        where user = User "Austin" "Erlandson" "austin@erlandson.com"
              key = toSqlKey 1 :: UserId


instance ToSample (Entity Venue) where
    toSamples _ = singleSample $ Entity key venue
        where venue = Venue (UTCTime (fromGregorian 2017 7 21) (secondsToDiffTime 0)) "Somthing"
              key = toSqlKey 1 :: VenueId


main :: IO ()
main = do
    echo "Writing: DOCS.md"
    writeFile "DOCS.md" (docsBS :: ByteString)


