port module Ports exposing (easeIntoView, snapIntoView)


port easeIntoView : Id -> Cmd msg


port snapIntoView : Id -> Cmd msg


type alias Id =
    String
