port module Stylesheets exposing (main)

import Css.File exposing (CssFileStructure, CssCompilerProgram, compile, compiler, toFileStructure)
import Platform
import Normalize
import Styles


port files : CssFileStructure -> Cmd msg


structure : CssFileStructure
structure =
    toFileStructure [ ( "main.css", compile [ Normalize.css, Styles.css ] ) ]


main : CssCompilerProgram
main = compiler files structure
