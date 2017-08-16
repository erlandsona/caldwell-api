{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Generators.ViewParser where

import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
import Data.Time (UTCTime(UTCTime), fromGregorian, secondsToDiffTime)
import qualified Data.Text.IO as IO
import Prelude as P
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree


import Models


main :: IO ()
main = do
    document <- IO.readFile "app/Server/TestView/snippet.html"
    document |>
        parseTree |>
        inject
            [ Account "Austin" "Erlandson" "austin@erlandson.com"
            , Account "Emily" "Kroll" "krollemily@ymail.com"
            ] |>
        inject
            [ Venue (UTCTime (fromGregorian 2017 7 21) (secondsToDiffTime 0)) "Belcourt Taps"
            , Venue (UTCTime (fromGregorian 2017 7 22) (secondsToDiffTime 0)) "Somewhere Else"
            ] |>
        renderTree |>
        print




inject :: (ToJSON deta) => [deta] -> [TagTree Text] -> [TagTree Text]
inject deta html = transformTree (injector deta html) html




injector :: (ToJSON deta) => [deta] -> [TagTree Text] -> TagTree Text -> [TagTree Text]
injector (datum:deta) (_:_:tags) tree@(TagBranch _ [(typ, key)] _) =
    case "data-" `isPrefixOf` typ of
        True -> case T.drop 5 typ of
            "string" -> injectable (decodeObject key datum) tree
            "list" -> tree : inject deta tags
            _ -> [tree]
        False -> [tree]
injector _ _ tree = [tree]




injectable :: Result Text -> TagTree Text -> [TagTree Text]
injectable result tree@(TagBranch name attrs _) =
    case result of
        Success v ->
            [ TagBranch name attrs
                [ TagLeaf (TagText v)
                ]
            ]
        Error _ -> [tree]
injectable _ tree = [tree]




decodeObject :: (ToJSON record) => Text -> record -> Result Text
decodeObject field record = parse parser value
    where
        parser = withObject (P.head . P.words . show $ value) $ do (.: field)
        value = toJSON record


(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>
