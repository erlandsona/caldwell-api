module Main.Styles exposing (css)

-- Libraries

import Css exposing (..)
import Css.Elements exposing (..)
import Lib.CssHelpers exposing (prop)


-- Source

import Constants exposing (caldwellCalendar, gutterSize)
import Types exposing (Page(..), HtmlClass(..), Nav(..))
import Constants exposing (..)


css : List Snippet
css =
    [ main_
        [ position relative
        , children
            [ section
                [ minHeight (vh 100)
                , width (pct 100)
                , maxWidth (px 768)
                  -- Tablet Width
                , margin auto
                , prop "-webkit-overflow-scrolling" "touch"
                , padding4
                    (Css.rem 7)
                    -- Top
                    (Css.rem <| gutterSize.numericValue * 3)
                    -- Right
                    zero
                    -- Bottom
                    gutterSize
                  -- Left
                ]
            , id Home
                [ displayFlex
                , alignItems flexEnd
                , paddingBottom gutterSize
                , maxWidth (pct 100)
                , height (vh 70)
                , descendants
                    [ a
                        [ display inlineBlock
                        , marginRight gutterSize
                        ]
                    , class "fa"
                        [ fontSize (Css.rem 1.5) ]
                    ]
                ]
            , id About
                [ children
                    [ h3 [ display inline ]
                    , p
                        [ prop "text-indent" "7%"
                        , lineHeight (num 1.5)
                        , marginBottom gutterSize
                        ]
                    ]
                ]
            , id Shows
                [ children
                    [ selector caldwellCalendar
                        [ display block
                        , children
                            [ h2
                                [ textAlign center
                                ]
                            , fadingHr lightGrey
                            , class Gigs
                                [ children
                                    [ mediaQuery "screen and ( max-width: 500px )"
                                        [ class Gig
                                            [ justifyContent spaceAround
                                            , children
                                                [ span
                                                    [ nthChild "2"
                                                        [ order (num -1)
                                                        , width (pct 100)
                                                        , textAlign center
                                                        , marginBottom (px 17)
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    , class Gig
                                        [ displayFlex
                                        , flexFlow2 row wrap
                                        , justifyContent spaceBetween
                                        ]
                                    ]
                                , children [ fadingHr darkGrey ]
                                ]
                            ]
                        ]
                    ]
                ]
            , id Music
                [ children
                    [ h2
                        [ textAlign center
                        ]
                    , selector "iframe"
                        [ width (pct 100) ]
                    , fadingHr lightGrey
                    , selector "fading-hr"
                        [ nthOfType "1n+2"
                            [ backgroundColor darkGrey
                            , prop "background" <| "-webkit-gradient(linear, 0 0, 100% 0, from(black), to(black), color-stop(50%, " ++ darkGrey.value ++ "))"
                            ]
                        ]
                    ]
                ]
            , id Contact
                [ children
                    [ h2
                        [ textAlign center
                        ]
                    , fadingHr lightGrey
                    , a
                        [ display block
                        , textAlign center
                        ]
                    ]
                ]
            ]
        ]
    ]


fadingHr : Color -> Snippet
fadingHr background =
    selector "fading-hr"
        [ display block
        , margin2 (px 20) zero
        , height (px 1)
        , width (pct 100)
        , backgroundColor background
        , prop "background" <| "-webkit-gradient(linear, 0 0, 100% 0, from(black), to(black), color-stop(50%, " ++ background.value ++ "))"
        ]
