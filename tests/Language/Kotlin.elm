module Language.Kotlin exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Kotlin as Kotlin exposing (Syntax(..), toRevTokens)
import Test exposing (..)


suite : Test
suite =
    describe "Kotlin Language Test Suite"
        [ fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run Kotlin.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
