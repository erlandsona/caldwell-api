{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ViewParser where

import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
-- import Data.Time (UTCTime(UTCTime), fromGregorian, secondsToDiffTime)
-- import qualified Data.Text.IO as IO
import Prelude as P
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree



compile :: (ToJSON deta) => Text -> [deta] -> Text
compile template deta =
    parseTree template
    & inject deta
    & renderTree



inject :: (ToJSON deta) => [deta] -> [TagTree Text] -> [TagTree Text]
inject deta html = transformTree (injector deta html) html




injector :: (ToJSON deta) => [deta] -> [TagTree Text] -> TagTree Text -> [TagTree Text]
injector (datum:deta) (_:_:tags) tree@(TagBranch _ [(typ, key)] _) =
    case "data-" `isPrefixOf` typ of
        True -> case T.drop 5 typ of
            "string" -> injectable (decodeObject key datum) tree
            "prop" -> tree : inject deta (injectable (decodeObject key datum) tree)
            "list" -> tree : inject deta tags
            _ -> [tree]
        False -> [tree]
injector _ _ tree = [tree]




injectable :: Result Text -> TagTree Text -> [TagTree Text]
injectable result tree@(TagBranch name attrs _) =
    case result of
        Success v ->
            [ TagBranch name attrs
                [ TagLeaf (TagText v)
                ]
            ]
        Error _ -> [tree]
injectable _ tree = [tree]




decodeObject :: (ToJSON record) => Text -> record -> Result Text
decodeObject field record = parse parser value
    where
        value = toJSON record
        parser = withObject (P.head . P.words . show $ value) $ do (.: field)


(&) :: a -> (a -> b) -> b
x & f = f x
infixl 0 &
