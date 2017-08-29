port module Ports exposing (..)

import Types exposing (ClassName)
import Scroll exposing (Move)


port easeIntoView : ClassName -> Cmd action


port snapIntoView : ClassName -> Cmd action


port scroll : (Move -> action) -> Sub action
