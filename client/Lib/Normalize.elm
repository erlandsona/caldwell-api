module Lib.Normalize exposing (css, snippets)

-- Copy Pasta from https://github.com/scottcorgan/elm-css-reset/blob/1.0.2/src/Css/Reset.elm#L96
-- until the version of elm-css get's updated
-- Libraries

import Css exposing (..)
import Css.Elements as Elem exposing (..)
import Lib.CssHelpers exposing (prop)
import String.Extra exposing (clean)


css : Stylesheet
css =
    stylesheet snippets


snippets : List Snippet
snippets =
    [ selector
        (clean
            """
            html, body
            , center, div, span
            , h1, h2, h3, h4, h5, h6
            , a, p
            , ol, ul, li
            , img
            , form
            , pre
            , fieldset, legend, caption
            , table, tbody, tfoot, thead
            , tr, th, td
            , label
            , article, aside, section
            , header, footer, nav
            , audio, video
            , strong
            , canvas, iframe
            , abbr, acronym
            , address, big
            , blockquote, cite
            , code, del
            , dl, dt, dd
            , dfn, em, ins, kbd
            , object
            , q, s
            , samp, small
            , b, u, i
            , strike, sub, sup, tt
            , var, details, embed
            , figure, figcaption
            , menu, output, ruby
            , summary, time, mark
            """
        )
        [ margin zero
        , padding zero
        , border zero
        , fontSize (pct 115)
        , lineHeight (int 1)
        , prop "-ms-text-size-adjust" "100%"
        , prop "-webkit-text-size-adjust" "100%"
        , prop "-webkit-font-smoothing" "antialiased"
        , verticalAlign baseline
        ]
    , each
        [ article
        , selector "aside"
        , selector "details"
        , selector "figcaption"
        , selector "figure"
        , footer
        , header
        , selector "menu"
        , nav
        , section
        ]
        [ display block ]
    , each
        [ ol, ul ]
        [ prop "list-style" "none" ]
    , html
        [ boxSizing borderBox ]
    , everything
        [ before [ boxSizing inherit ]
        , boxSizing inherit
        , after [ boxSizing inherit ]
        ]
    , a
        [ backgroundColor transparent
        , prop "-webkit-text-decoration-skip" "objects"
        , active
            [ prop "outline-width" "0"
            ]
        , hover
            [ prop "outline-width" "0"
            ]
        ]
    , img [ borderStyle none ]
    , each [ button, input, optgroup, select, selector "textarea" ]
        [ fontFamilies [ "sans-serif" ]
        , fontSize (pct 100)
        , lineHeight (int 1)
        , margin zero
        ]
    , each [ button, input ] [ overflow visible ]
    , each [ button, select ] [ prop "text-transform" "none" ]
    , each
        [ button
        , selector "html [type=\"button\"]"
        , selector "[type=\"reset\"]"
        , selector "[type=\"submit\"]"
        ]
        [ prop "-webkit-appearance" "button" ]
    , each
        [ selector "button::-moz-focus-inner"
        , selector "[type=\"button\"]::-moz-focus-inner"
        , selector "[type=\"reset\"]::-moz-focus-inner"
        , selector "[type=\"submit\"]::-moz-focus-inner"
        ]
        [ borderStyle none
        , padding zero
        ]
    , each
        [ selector "button::-moz-focusring"
        , selector "[type=\"button\"]::-moz-focusring"
        , selector "[type=\"reset\"]::-moz-focusring"
        , selector "[type=\"submit\"]::-moz-focusring"
        ]
        [ prop "outline" "1px dotted ButtonText" ]
    , fieldset
        [ border3 (px 1) solid (hex "c0c0c0")
        , margin2 zero (px 2)
        , padding3 (Css.em 0.35) (Css.em 0.625) (Css.em 0.75)
        ]
    , legend
        [ boxSizing borderBox
        , prop "color" "inherit"
        , prop "display" "table"
        , maxWidth (pct 100)
        , padding zero
        , prop "white-space" "normal"
        ]
    , selector "progress"
        [ display inlineBlock
        , verticalAlign baseline
        ]
    , selector "textarea" [ overflow auto ]
    , each
        [ selector "[type=\"checkbox\"]"
        , selector "[type=\"radio\"]"
        ]
        [ boxSizing borderBox
        , padding zero
        ]
    , each
        [ selector "[type=\"number\"]::-webkit-inner-spin-button"
        , selector "[type=\"number\"]::-webkit-outer-spin-button"
        ]
        [ height auto ]
    , selector "[type=\"search\"]"
        [ prop "-webkit-appearance" "textfield"
        , prop "outline-offset" "-2px"
        ]
    , each
        [ selector "[type=\"search\"]::-webkit-search-cancel-button"
        , selector "[type=\"search\"]::-webkit-search-decoration"
        ]
        [ prop "-webkit-appearance" "none" ]
    , selector "::-webkit-file-upload-button"
        [ prop "-webkit-appearance" "button"
        , prop "font" "inherit"
        ]
    , each [ selector "details", selector "menu" ] [ display block ]
    , selector "summary" [ prop "display" "list-item" ]
    , canvas [ display inlineBlock ]
    , selector "template" [ display none ]
    , selector "[hidden]" [ display none ]
    , selector "figcaption figure, main" [ display block ]
    , selector "figure" [ margin2 (Css.em 1) (px 40) ]
    , hr
        [ boxSizing contentBox
        , height zero
        , overflow visible
        ]
    , pre
        [ fontFamilies [ "monospace", "monospace" ]
        , fontSize (Css.em 1)
        ]
    , each [ selector "b", strong ] [ fontWeight inherit ]
    , selector "abbr[title]"
        [ prop "border-bottom" "none"
          -- borderBottom doesn't accept none
        , textDecoration underline
        , textDecoration2 underline dotted
        ]
    , each [ code, selector "kbd", selector "samp" ]
        [ fontFamilies [ "monospace", "monospace" ]
        , fontSize (Css.em 1)
        ]
    , selector "dfn" [ fontStyle italic ]
    , selector "mark"
        [ backgroundColor (hex "ff0")
        , color (hex "000")
        ]
    , selector "small" [ fontSize (pct 80) ]
    , each [ selector "sub", selector "sup" ]
        [ fontSize (pct 75)
        , lineHeight zero
        , position relative
        , verticalAlign baseline
        ]
    , selector "sub" [ bottom (Css.em -0.25) ]
    , selector "sup" [ top (Css.em -0.5) ]
    , each [ audio, video ] [ display inlineBlock ]
    , selector "audio:not([controls])"
        [ display none
        , height zero
        ]
    , selector "svg:not(:root)" [ overflow hidden ]
    , each
        [ selector "blockquote", selector "q" ]
        [ prop "quotes" "none" ]
    , selector "blockquote:before, blockquote:after, q:before, q:after"
        [ prop "content" "\"\""
        , prop "content" "none"
        ]
    , Elem.table
        [ prop "border-collapse" "collapse"
        , prop "border-spacing" "0"
        ]
    , selector
        (clean
            """
            [role="button"]
            , input[type="submit"]
            , input[type="reset"]
            , input[type="button"]
            , button
            """
        )
        [ prop "-webkit-box-sizing" "content-box"
        , prop "-moz-box-sizing" "content-box"
        , boxSizing contentBox
        , prop "background" "none"
        , border zero
        , color inherit
        , cursor default
        , prop "line-height" "normal"
        , overflow visible
        , padding zero
        , textAlign center
        , textDecoration none
        , prop "white-space" "pre"
        , prop "-webkit-appearance" "button"
        , prop "-webkit-user-select" "none"
        , prop "-moz-user-select" "none"
        , prop "-ms-user-select" "none"
        ]
    ]
