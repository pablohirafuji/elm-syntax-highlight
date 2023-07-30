module Language.Nix exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Nix as Nix exposing (Syntax(..), toRevTokens)
import Test exposing (..)


suite : Test
suite =
    describe "Nix Language Test Suite"
        [ fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run Nix.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
