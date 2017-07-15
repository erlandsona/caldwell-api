module Main where

import Lib

spec :: Spec
spec = Spec ["Server"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy Book)
             : toElmDecoderSource (Proxy :: Proxy Book)
             : generateElmForAPI  (Proxy :: Proxy BooksApi))

main :: IO ()
main = specsToDir [spec] "client"
