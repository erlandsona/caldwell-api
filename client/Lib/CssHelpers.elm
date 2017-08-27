module Lib.CssHelpers exposing (..)

import Css exposing (Mixin, property)


-- Css Helpers


prop : String -> String -> Mixin
prop =
    property



-- shadowList : Int -> Float -> String
-- shadowList multiplier thickness =
--     let
--         ratio = 3
--         forgroundShadowValues =
--             List.map (\blur -> "0px 0px " ++ (toString blur) ++ "px white, ") <|
--             List.map ((*) (multiplier // 2)) (List.range 1 <| Basics.round <| logBase ratio thickness)
--         backgroundShadowValues =
--             List.map (\blur -> "0px 0px " ++ (toString blur) ++ "px black, ") <|
--             List.map ((*) multiplier) (List.range 1 <| Basics.round thickness)
--         dropComma = trim >> dropRight 1
--     in
--           ((String.concat forgroundShadowValues) |> trim)
--         ++ (String.concat backgroundShadowValues |> dropComma)
