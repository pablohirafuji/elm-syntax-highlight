module Main exposing (suite)

import Test exposing (..)
import Language.Elm
import Language.Xml


--import Language.Javascript


suite : Test
suite =
    describe "Elm Syntax Highlight Test Suite"
        [ Language.Elm.suite
        , Language.Xml.suite

        --, Language.Javascript.suite
        ]
