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

    parseTree document |>
        injectHaskellIntoHtmlAST
            [ Account "Austin" "Erlandson" "austin@erlandson.com"
            , Account "Emily" "Kroll" "krollemily@ymail.com"
            ] |>
        renderTree |>
        print


injectHaskellIntoHtmlAST :: [Account] -> [TagTree Text] -> [TagTree Text]
injectHaskellIntoHtmlAST (acct:accts) html@(tag:tags) = transformTree injector html
    where
        injector x@(TagBranch name attrs@[(typ, key)] _) =
            case "data-" `isPrefixOf` typ of
                True -> case T.drop 5 typ of
                    "string" -> injectable
                    "list" -> injectHaskellIntoHtmlAST accts tags ++ [x]
                False -> [x]
            where
                injectable =
                    case decodeObject key acct of
                        Success v ->
                            [ TagBranch name attrs
                                [ TagLeaf (TagText v)
                                ]
                            ]
                        Error _ -> [x]
        injector x = [x]
injectHaskellIntoHtmlAST [] html = html


decodeObject :: (ToJSON record) => Text -> record -> Result Text
decodeObject field record = parse parser value
    where
        parser = withObject (P.head . P.words . show $ value) $ do (.: field)
        value = toJSON record


x |> f = f x
infixl 0 |>
