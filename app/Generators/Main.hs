module Generators.Main where

-- import Data.Text as T
import Elm
-- import System.Process

import Generators.Elm

main :: IO ()
main = do
    -- elmFormat <- createProcess (proc "elm-format" ["--yes", "--stdin"])
    --     { std_in = CreatePipe
    --     , std_out = CreatePipe
    --     }
    -- let formattedSpec = elmFormat $ T.unlines elmText
    specsToDir [spec] "app/Client"

