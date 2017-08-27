module Constants exposing (..)

-- Libraries

import Css exposing (hex, Color, Rem)


-- Paddings / Margins


gutterSize : Rem
gutterSize =
    Css.rem 1


titleHeight : Rem
titleHeight =
    Css.rem 2.7



-- Colors


white : Color
white =
    hex "#FFFFFF"


lightGrey : Color
lightGrey =
    hex "#AAAAAA"


grey : Color
grey =
    hex "#777777"


darkGrey : Color
darkGrey =
    hex "#333333"


black : Color
black =
    hex "#000000"



-- Cross FileType Names / Module Namespaces
-- (EG: HTML / CSS selectors with same names.)


caldwellBackground : String
caldwellBackground =
    "caldwell-background"


caldwellCalendar : String
caldwellCalendar =
    "caldwell-calendar"


blackOverlay : String
blackOverlay =
    "black-overlay"


container : String
container =
    "container"


homepage : String
homepage =
    "homepage-"
