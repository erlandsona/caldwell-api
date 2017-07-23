module Main where

import Elm

import Generators.Elm

main :: IO ()
main = do
    specsToDir [spec] "app/Client"
