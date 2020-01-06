module Language.Json exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Json as Json exposing (Syntax(..), toRevTokens)
import Test exposing (..)


suite : Test
suite =
    describe "JSON Language Test Suite"
        [ fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run Json.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
