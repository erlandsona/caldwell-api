module Main (main) where

import Distribution.Simple
import System.Process

-- import Elm (specsToDir)

-- import Client.Generator (spec)

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { postCopy = generateElmFile }


generateElmFile _ _ _ _ = do
    _ <- createProcess (shell "caldwell-elm")
    return ()
