module Nav.View exposing (template)

-- Libs

import Css exposing (transform, translate2, translate3d, zero)
import Html exposing (..)
import Html.CssHelpers exposing (withNamespace)
import Html.Events exposing (onClick, onWithOptions, Options)
import Json.Decode exposing (succeed)


-- Source

import Types exposing (..)
import Constants exposing (..)


{ class } =
    withNamespace homepage


template : Nav -> Html Action
template navState =
    nav
        [ styles
            (if navState == Open then
                [ transform (translate2 zero zero)
                , transform (translate3d zero zero zero)
                ]
             else
                []
            )
        , class [ NavBar () ]
        , clickWithStopProp (Toggle Closed)
        ]
        [ aTag Home
        , aTag About
        , aTag Shows
        , aTag Music
        , aTag Contact
        , ul [ class [ NavBar "handle" ], clickWithStopProp (Toggle <| not navState) ]
            [ li [] [] ]
        ]


aTag : Page -> Html Action
aTag page =
    a [ class [ (NavBar "a") ], onClick (SetUrl page) ]
        [ span [] [ text (toString page) ]
        ]


not : Nav -> Nav
not navState =
    case navState of
        Open ->
            Closed

        Closed ->
            Open


clickWithStopProp : Action -> Attribute Action
clickWithStopProp action =
    onWithOptions "click" (Options True False) (succeed action)



-- Options stopProp prevDefault
