{-# LANGUAGE OverloadedStrings #-}


module Generators.ViewParser where

import Text.XML.HXT.Core
import Text.HandsomeSoup


main :: IO ()
main = do
    rawTemplate <- readFile "app/Server/TestView/snippet.html"

    -- parsedTemplate <- parseHtml rawTemplate
    -- runX . xshow $ render rawTemplate "Data!"
    -- runX $ parsedTemplate >>> css "[data-care]" /> changeText (foldl (\_ -> return "Stuff") "")
    -- Works in a console but won't compile inside IO :shrug:
    -- _ <- runLA (hread >>> css "[data-care]" /> changeText (foldl (\_ -> return "Stuff") "")) rawTemplate
    return ()




render template datum = 
    (parseHtml template) >>> css "[data-bucketOfCares]" /> changeText (map (\_->datum))
