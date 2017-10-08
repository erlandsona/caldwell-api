port module Stylesheets exposing (main)

import Css.File exposing (CssFileStructure, CssCompilerProgram, compile, compiler, toFileStructure)
import Platform
import Lib.Normalize as Normalized
import Styles


port files : CssFileStructure -> Cmd action


structure : CssFileStructure
structure =
    toFileStructure [ ( "main.css", compile [ Normalized.css, Styles.css ] ) ]


main : CssCompilerProgram
main =
    compiler files structure
