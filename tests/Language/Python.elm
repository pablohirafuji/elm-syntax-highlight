module Language.Python exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Test exposing (..)
import SyntaxHighlight.Language.Python as Python exposing (Syntax(..), toRevTokens)


suite : Test
suite =
    describe "Python Language Test Suite"
        [ fuzz string "Fuzz string" <|
            \fuzzStr ->
                Python.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
