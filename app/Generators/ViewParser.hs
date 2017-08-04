{-# LANGUAGE OverloadedStrings #-}

module Generators.ViewParser where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.HTML.TagSoup.Tree
import Text.StringLike


import Models


main :: IO ()
main = do
    document <- readFile "app/Server/TestView/snippet.html"

    print $ renderTree $ injectHaskellIntoHtmlAST (show (User "Austin" "Erlandson" "austin@erlandson.com")) $ attributes document


attributes :: String -> [TagTree String]
attributes document = [x | x@(TagBranch _ [("data-key", _)] _) <- universeTree $ parseTree document ]

injectHaskellIntoHtmlAST :: String -> [TagTree String] -> [TagTree String]
injectHaskellIntoHtmlAST haskell tags =
    map (\tag@(TagBranch a b _) ->
        case b of
            [(c, "cares")] -> TagBranch a b [TagLeaf (TagText haskell)]
            _ -> tag
    ) tags


-- parseTree
-- [ TagBranch "section" [("data-key","sectionStuff")]
--     [ TagLeaf (TagText "\n      ")
--     , TagBranch "ul" [("data-key","cares")]
--         [ TagLeaf (TagText "\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
--         , TagLeaf (TagText "\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
--         , TagLeaf (TagText "\n       ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
--         , TagLeaf (TagText "\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")]
--         , TagLeaf (TagText "\n      ")
--         ]
--     , TagLeaf (TagText "\n    ")
--     ]
-- , TagBranch "ul" [("data-key","cares")]
--     [ TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
--     , TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
--     , TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
--     , TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")]
--     , TagLeaf (TagText "\n      ")
--     ]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "SomeThings Given")]
-- ]

-- [ TagBranch "ul" [("data-key","cares")]
--     [ TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf(TagText "No F***'s Given")], TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")], TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")], TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")], TagLeaf (TagText "\n      ")
--     ]
-- ]

-- optTagTextMerge = True with universeTree filtering
-- [ TagBranch "section" [("data-key","sectionStuff")]
--     [ TagLeaf (TagText "\n      ")
--     , TagBranch "ul" [("data-key","cares")]
--         [ TagLeaf (TagText "\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
--         , TagLeaf(TagText "\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
--         , TagLeaf (TagText "\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
--         , TagLeaf (TagText"\n        ")
--         , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")]
--         , TagLeaf (TagText "\n")
--         ]
--     , TagLeaf (TagText "\n    ")
--     ]
-- , TagBranch "ul" [("data-key","cares")]
--     [ TagLeaf (TagText "\n        ")
--     , TagBranch "li"[("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
--     , TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
--     , TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
--     , TagLeaf (TagText "\n        ")
--     , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")]
--     , TagLeaf (TagText "\n      ")
--     ]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
-- , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")]
-- ]

-- parseTreeOptions optTagTextMerge = True
-- [ TagLeaf (TagOpen "!DOCTYPE" [("html","")]), TagLeaf (TagText "\n")
-- , TagBranch "html" []
--     [ TagLeaf (TagText "\n  ")
--     , TagBranch "head" []
--         [ TagLeaf (TagText "\n    ")
--         , TagBranch "meta"[("charset","utf-9")] [], TagLeaf (TagText "\n    ")
--         , TagBranch "meta" [("name","viewport"),("content","width=device-width")] [],TagLeaf (TagText "\n    ")
--         , TagBranch "title" [] [TagLeaf (TagText "Test Template")], TagLeaf (TagText "\n    ")
--         , TagLeaf (TagOpen "link" [("rel","stylesheet"),("href","/css/master.css"),("type","text/css"),("media","screen"),("title","no title"),("charset","utf-8")]), TagLeaf (TagText "\n  ")
--         ]
--     , TagLeaf (TagText "\n  ")
--     , TagBranch "body" []
--         [ TagLeaf (TagText "\n    ")
--         , TagBranch "section" [("data-key","sectionStuff")]
--             [ TagLeaf (TagText "\n      ")
--             , TagBranch "ul" [("data-key","cares")]
--                 [ TagLeaf (TagText "\n        ")
--                 , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "No F***'s Given")]
--                 , TagLeaf (TagText "\n        ")
--                 , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Maybe S**t's Given")]
--                 , TagLeaf (TagText "\n        ")
--                 , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Definitely Handouts Given")]
--                 , TagLeaf (TagText "\n        ")
--                 , TagBranch "li" [("data-key","care")] [TagLeaf (TagText "Some Things Given")]
--                 , TagLeaf (TagText "\n      ")
--                 ]
--             , TagLeaf (TagText "\n    ")
--             ]
--         , TagLeaf (TagText "\n\n\n    ")
--         , TagBranch "p" [] [ TagLeaf (TagText "Some Content for your list my friend") ]
--         , TagLeaf (TagText "\n  ")
--         ]
--     , TagLeaf (TagText "\n")
--     ]
-- , TagLeaf (TagText "\n\n")
-- ]

-- [ TagOpen "section" [("data-key","sectionStuff")], TagText "\n      "
-- ,    TagOpen "ul" [("data-key","cares")], TagText "\n      "
-- ,       TagOpen "li" [("data-key","care")], TagText "No F***'s Given", TagClose "li", TagText "\n        "
-- ,       TagOpen "li" [("data-key","care")], TagText "Maybe S**t's Given", TagClose "li", TagText "\n        "
-- ,       TagOpen "li" [("data-key","care")], TagText "Definitely Handouts Given", TagClose "li", TagText "\n        "
-- ,       TagOpen "li" [("data-key","care")], TagText "Some Things Given", TagClose "li", TagText "\n      "
-- ,    TagClose "ul", TagText "\n    "
-- , TagClose "section"
-- , TagOpen "ul"[("data-key","cares")],TagText "\n        ",TagOpen "li" [("data-key","care")],TagText "No F***'s Given",TagClose "li",TagText "\n        ",TagOpen "li" [("data-key","care")],TagText "Maybe S**t's Given",TagClose "li",TagText "\n   ",TagOpen "li" [("data-key","care")],TagText "Definitely Handouts Given",TagClose "li",TagText "\n        ",TagOpen "li" [("data-key","care")],TagText "Some Things Given",TagClose "li",TagText "\n      ",TagClose "ul",TagOpen "li"[("data-key","care")],TagText "No F***'s Given",TagClose "li",TagOpen "li" [("data-key","care")],TagText "Maybe S**t's Given",TagClose "li",TagOpen "li" [("data-key","care")],TagText "Definitely Handouts Given",TagClose "li",TagOpen "li" [("data-key","care")],TagText "Some Things Given",TagClose "li"]
--
-- ^ flattenTree same as |
--                       V

-- parseTags
-- [ TagOpen "!DOCTYPE" [("html","")], TagText "\n"
-- , TagOpen "html" [], TagText "\n  "
-- , TagOpen "head" [], TagText "\n    "
-- ,    TagOpen "meta" [("charset","utf-8")], TagClose "meta", TagText "\n    "
-- ,    TagOpen "meta" [("name","viewport"),("content","width=device-width")], TagClose "meta", TagText "\n    "
-- ,    TagOpen "title" [], TagText "Test Template", TagClose "title", TagText "\n    "
-- ,    TagOpen "link" [("rel","stylesheet"),("href","/css/master.css"),("type","text/css"),("media","screen"),("title","no title"),("charset","utf-8")], TagText "\n  "
-- , TagClose "head", TagText "\n  "
-- , TagOpen "body" [], TagText "\n    "
-- ,    TagOpen "section" [("data-key","sectionStuff")], TagText "\n      "
-- ,        TagOpen "ul" [("data-key","cares")], TagText"\n        "
-- ,            TagOpen "li" [("data-key","care")]
-- ,                TagText "No F***'s Given"
-- ,            TagClose "li", TagText "\n        "
-- ,            TagOpen"li" [("data-key","care")]
-- ,                TagText "Maybe S**t's Given"
-- ,            TagClose "li", TagText "\n        "
-- ,            TagOpen "li" [("data-key","care")]
-- ,                TagText "Definitely Handouts Given"
-- ,            TagClose "li", TagText "\n        "
-- ,            TagOpen "li" [("data-key","care")]
-- ,                TagText "Some Things Given"
-- ,            TagClose "li", TagText "\n      "
-- ,        TagClose "ul", TagText "\n    "
-- ,     TagClose "section", TagText"\n\n\n    "
-- ,     TagOpen "p" []
-- ,         TagText "Some Content for your list my friend"
-- ,     TagClose "p", TagText "\n  "
-- , TagClose "body", TagText "\n"
-- , TagClose "html", TagText "\n\n"
-- ]
