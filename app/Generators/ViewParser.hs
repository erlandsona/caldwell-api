module Generators.ViewParser where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match



main :: IO ()
main = do
    document <- readFile "app/Server/TestView/snippet.html" 
    print $ filter (\tag ->
                        if isTagOpen tag
                        then anyAttr "<li data-care>"
                        else tag
                    ) $ parseTags document

    -- print $ render ast "Stuff"


