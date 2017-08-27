port module Ports exposing (easeIntoView, snapIntoView)

port easeIntoView : ID -> Cmd msg

port snapIntoView : ID -> Cmd msg

type alias ID = String
