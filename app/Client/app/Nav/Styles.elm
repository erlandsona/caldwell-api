module Nav.Styles exposing (css)

-- Libraries

import Css exposing (..)
import Css.Elements exposing (..)
import CssHelpers exposing (prop)


-- Source

import Constants exposing (..)


css : List Snippet
css =
    [ nav
        [ displayFlex
        , flexDirection column
        , prop "justify-content" "space-around"
        , prop "align-items" "flex-end"
        , prop "transition" "0.3s"
        , position fixed
        , right zero
        , cursor pointer
        , height (vh 50)
        , margin2 (vh 25) zero
        , backgroundColor transparent
        , prop "-ms-transform" "translateX(110%)"
        , prop "transform" <| "translate3d(calc(100% + "++gutterSize.value++"), 0, 0)"
        , zIndex (int 1)
        , children
            [ a
                [ cursor pointer
                , displayFlex
                , flexDirection column
                , position relative
                , flexGrow (int 1)
                , prop "justify-content" "flex-end"
                , textDecoration none
                , borderBottom3 (px 1) solid transparent
                , prop "transition" "0.25s"
                , width (pct 0)
                , prop "white-space" "nowrap"
                , textAlign left
                , prop "direction" "rtl"
                , prop "text-shadow" "0px 0px 7px white"
                , fontSize (pct 175)
                , fontFamily sansSerif
                , fontFamilies [ "Megrim" ]
                , hover
                    [ borderBottom2 (px 1) solid
                    , width (pct 110)
                    ]
                , children
                    [ span
                        [ marginRight gutterSize ]
                    ]
                ]
            , ul
                [ displayFlex
                , flexDirection column
                , prop "justify-content" "space-around"
                , prop "align-items" "center"
                , position absolute
                , top (pct 47)
                , height (px <| dotSize * 3)
                , width (px <| dotSize * 3)
                , prop "right" "calc(110% + 1rem)"
                , children
                    [ li
                        [ width (px dotSize)
                        , height (px dotSize)
                        , backgroundColor white
                        , prop "box-shadow" "0px 0px 7px white"
                        , borderRadius (pct 50)
                        ]
                    ]
                ]
            ]
        ]
    ]

dotSize : Float
dotSize = 13
