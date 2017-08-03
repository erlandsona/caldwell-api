module Generators.ViewParser where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree



main :: IO ()
main = do
    document <- readFile "app/Server/TestView/snippet.html" 
    let attributes = [x | x@(TagBranch _ [("data-care", _)] _) <- universeTree (parseTree document) ]
    print attributes

    -- print $ render ast "Stuff"


