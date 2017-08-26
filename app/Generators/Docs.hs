{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Prelude hiding (writeFile)
import Servant.Docs
import Turtle

import Routes
import Models



docsBS :: ByteString
docsBS = encodeUtf8
       . pack
       . markdown
       $ docsWithIntros [intro] (Proxy :: Proxy Router)

  where intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]


instance ToSample Account where
    toSamples _ = singleSample $ Account "Austin" "Erlandson" "austin@erlandson.com"


instance ToSample Gig where
    toSamples _ = singleSample $ Gig (UTCTime (fromGregorian 2017 7 21) (secondsToDiffTime 0)) "Somthing"


main :: IO ()
main = do
    echo "Writing: DOCS.md"
    writeFile "DOCS.md" (docsBS :: ByteString)


