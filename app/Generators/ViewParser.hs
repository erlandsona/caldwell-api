{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Generators.ViewParser where

import Data.Aeson
import Data.Aeson.Types
import Data.HashMap.Strict
import Data.Text as T
import qualified Data.Text.IO as IO
import Prelude as P hiding (pack)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Text.StringLike


import Models


main :: IO ()
main = do
    document <- IO.readFile "app/Server/TestView/snippet.html"
    document |>
        parseTree |>
        injectHaskellIntoHtmlAST
            [ Account "Austin" "Erlandson" "austin@erlandson.com"
            , Account "Emily" "Kroll" "krollemily@ymail.com"
            ] |>
        renderTree |>
        print




injectHaskellIntoHtmlAST :: [Account] -> [TagTree Text] -> [TagTree Text]
injectHaskellIntoHtmlAST accts html@(_:_:tags) = transformTree (injector accts tags) html
injectHaskellIntoHtmlAST [] html = html




injector :: [Account] -> [TagTree Text] -> TagTree Text -> [TagTree Text]
injector (acct:accts) tags tree@(TagBranch name attrs@[(typ, key)] _) =
    case "data-" `isPrefixOf` typ of
        True -> case T.drop 5 typ of
            "string" -> injectable (decodeObject key acct) tree
            "list" -> tree : injectHaskellIntoHtmlAST accts tags
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




decodeObject :: (ToJSON record) => Text -> record -> Result Text
decodeObject field record = parse parser value
    where
        parser = withObject (P.head . P.words . show $ value) $ do (.: field)
        value = toJSON record


x |> f = f x
infixl 0 |>
