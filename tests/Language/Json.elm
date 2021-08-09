module Language.Json exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Json as Json exposing (Syntax(..), toRevTokens)
import SyntaxHighlight.Language.Type as T exposing (Syntax(..))
import Test exposing (..)


suite : Test
suite =
    describe "JSON Language Test Suite"
        [ numberTest "integer zero" "0"
        , numberTest "decimal zero" "0.0"
        , numberTest "life, the universe, everything" "42"
        , numberTest "negative value" "-1"
        , numberTest "negative decimal" "-1.2345"
        , numberTest "Euler's number" "2.71828"
        , numberTest "Avogadro's number exponent (E) notation" "6.0221409E23"
        , numberTest "Avogadro's number exponent (e) notation" "6.0221409e23"
        , numberTest "Avogadro's number exponent (E+) notation" "6.0221409E+23"
        , numberTest "Avogadro's number exponent (e+) notation" "6.0221409e+23"
        , numberTest "Gauss' constant exponent (E) notation" "8.346268E-1"
        , numberTest "Gauss' constant exponent (e) notation" "8.346268e-1"
        , fuzz string "Fuzz string" <|
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


numberTest : String -> String -> Test
numberTest testName testStr =
    describe ("Number :" ++ testName)
        [ equalTest "number"
            ( "{\"number\": " ++ testStr ++ "}" )
            ( Ok [(C Object,"{"),(C ObjectKey,"\""),(C ObjectKey,"number"),(C ObjectKey,"\""),(C Object,":"),(Normal," "),(C Number,testStr),(C Object,"}")])
        ]


equalTest : String -> String -> Result (List Parser.DeadEnd) (List ( T.Syntax Json.Syntax, String )) -> Test
equalTest testName testStr testResult =
    describe testName
        [ test "Syntax equality" <|
            \() ->
                Parser.run Json.toRevTokens testStr
                    |> Result.map List.reverse
                    |> equal testResult
        , test "String equality" <|
            \() ->
                Parser.run Json.toRevTokens testStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok testStr)
        ]
