module Language.Python exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Python as Python exposing (Syntax(..), toRevTokens)
import Test exposing (..)


suite : Test
suite =
    describe "Python Language Test Suite"
        [ fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run Python.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
