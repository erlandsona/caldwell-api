{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Generators.ViewParser where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict
import Data.Text as T
import qualified Data.Text.IO as IO
import Prelude hiding (pack)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Text.StringLike


import Models
import Validator


listFields ''Account


main :: IO ()
main = do
    print $ Account { firstName = "Austin", lastName = "Erlandson", email = "austin@erlandson.com"}
    -- document <- IO.readFile "app/Server/TestView/snippet.html"

    -- parseTree document |>
    --     injectHaskellIntoHtmlAST (Account "Austin" "Erlandson" "austin@erlandson.com") |>
    --     renderTree |>
    --     print


injectHaskellIntoHtmlAST :: Account -> [TagTree Text] -> [TagTree Text]
injectHaskellIntoHtmlAST haskell = transformTree injector
    where
        injector x@(TagBranch name attrs@[(key, val)] _) =
            case "data-" `isPrefixOf` key of
                True -> injectable
                False -> [x]
            where
                injectable = [TagBranch name attrs [TagLeaf (TagText $ decodeObject prop (toJSON haskell))]]
                prop = T.drop 5 key
        injector x = [x]


decodeObject :: Text -> Value -> Text
decodeObject field record =
  let parser = withObject (Prelude.head . Prelude.words . show $ record) $ \o -> do
        val <- o .: field
        return val
      result = parse parser record
  in case result of
    Error _   -> "IT BROKE"
    Success v -> v


x |> f = f x
infixl 0 |>
