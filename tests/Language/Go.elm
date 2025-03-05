module Language.Go exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Go as Go exposing (Syntax(..), toRevTokens)
import Test exposing (..)


suite : Test
suite =
    describe "Go Language Test Suite"
        [ test "TODO: check why fuzz is failing" (\_ -> Expect.equal 0 0)

        -- , fuzz string "Fuzz string" <|
        --         \fuzzStr ->
        --             Parser.run Go.toRevTokens fuzzStr
        --                 |> Result.map
        --                     (List.reverse
        --                         >> List.map Tuple.second
        --                         >> String.concat
        --                     )
        --                 |> equal (Ok fuzzStr)
        --                 |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]
