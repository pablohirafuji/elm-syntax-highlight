module Language.NoLang exposing (suite)

import Expect exposing (equal, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.NoLang as NoLang exposing (Syntax(..), toRevTokens)
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "NoLang Language Test Suite"
        [ fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run NoLang.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
