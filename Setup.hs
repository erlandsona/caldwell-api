module Main (main) where

import Distribution.Simple
import System.Process

main :: IO ()
main = defaultMainWithHooks $
    simpleUserHooks { postCopy = generateElmFile }


generateElmFile = do
    _ <- createProcess (shell "caldwell-elm")
    return ()
