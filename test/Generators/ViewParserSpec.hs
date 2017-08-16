{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Generators.ViewParserSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.IO as IO
import NeatInterpolation
import Test.Hspec



import Models
import ViewParser as VP

spec :: Spec
spec = do
  describe "compile" $ do
    it "Matches the Test case" $ do
      let document = [text|<div data-prop="firstName">Stuff</div>|]
      let deta =
            [ Account "Austin" "Erlandson" "austin@erlandson.com"
            --       firstName  lastName     email
            , Account "Emily" "Kroll" "krollemily@ymail.com"
            ]
      let result = VP.compile document deta
      result `shouldBe` expected

expected = [text|
  <div data-prop="firstName">Austin</div>
  <div data-prop="firstName">Emily</div>
|]

expected2 = [text|
  <article data-list="account">
    <div class="my-wrapper">
      <span data-account="firstName">My First</span>
    </div>
    <div class="other-wrapper">
      <span data-account="lastName">My Last</span>
      <span data-account="email">My Email</span>
    </div>
  </article>
|]

expected3 = [text|
  <ul data-list="venue">
    <li data-date">Date</li>
    <li data-location">Location</li>
  </ul>
|]
