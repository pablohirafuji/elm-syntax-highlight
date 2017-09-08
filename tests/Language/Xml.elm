module Language.Xml exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal, onFail)
import Fuzz exposing (string)
import Test exposing (..)
import Parser
import SyntaxHighlight.Language.Xml as Xml exposing (..)


suite : Test
suite =
    describe "Xml Language Test Suite"
        [ equalTest "Element" "<p class=\"hero\">Hero</p>" <|
            Ok [ ( Normal, "<" ), ( Tag, "p" ), ( Normal, " " ), ( Attribute, "class" ), ( Normal, "=" ), ( AttributeValue, "\"" ), ( AttributeValue, "hero" ), ( AttributeValue, "\"" ), ( Normal, ">Hero" ), ( Normal, "</" ), ( Tag, "p" ), ( Normal, ">" ) ]
        , equalTest "Comment" "<script>\n<!-- comment\n-->\n</script" <|
            Ok [ ( Normal, "<" ), ( Tag, "script" ), ( Normal, ">" ), ( LineBreak, "\n" ), ( Comment, "<!--" ), ( Comment, " comment" ), ( LineBreak, "\n" ), ( Comment, "-->" ), ( LineBreak, "\n" ), ( Normal, "</" ), ( Tag, "script" ) ]
        , equalTest "Incomplete tag" "<html" <|
            Ok [ ( Normal, "<" ), ( Tag, "html" ) ]
        , equalTest "Incomplete comment" "<!-- comment" <|
            Ok [ ( Comment, "<!--" ), ( Comment, " comment" ) ]
        , equalTest "Incomplete attribute" "<div class" <|
            Ok [ ( Normal, "<" ), ( Tag, "div" ), ( Normal, " " ), ( Attribute, "class" ) ]
        , equalTest "Incomplete attribute 2" "<div class =" <|
            Ok [ ( Normal, "<" ), ( Tag, "div" ), ( Normal, " " ), ( Attribute, "class" ), ( Normal, " " ), ( Normal, "=" ) ]
        , fuzz string "The result should always be Ok" <|
            \fuzzStr ->
                Xml.toSyntax fuzzStr
                    |> Result.map (always [])
                    |> equal (Ok [])
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]


equalTest : String -> String -> Result Parser.Error (List ( SyntaxType, String )) -> Test
equalTest testName testStr testResult =
    test testName <|
        \() ->
            Xml.toSyntax testStr
                |> Result.map List.reverse
                |> equal testResult
