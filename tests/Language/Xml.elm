module Language.Xml exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal)
import Test exposing (..)
import SyntaxHighlight.Language.Xml as Xml exposing (..)


suite : Test
suite =
    describe "Xml Language Test Suite"
        [ test "Element" <|
            \() ->
                Xml.toSyntax "<script src=\"main.js\">nothing</script>"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( Normal, "<" ), ( Tag, "script" ), ( Normal, " " ), ( Attribute, "src" ), ( Normal, "=" ), ( AttributeValue, "\"" ), ( AttributeValue, "main.js" ), ( AttributeValue, "\"" ), ( Normal, ">" ), ( Normal, "nothing" ), ( Normal, "</" ), ( Tag, "script" ), ( Normal, ">" ) ])
        , test "Comment" <|
            \() ->
                Xml.toSyntax "<script>\n<!-- comment\n-->\n</script"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( Normal, "<" ), ( Tag, "script" ), ( Normal, ">" ), ( LineBreak, "\n" ), ( Comment, "<!--" ), ( Comment, " comment" ), ( LineBreak, "\n" ), ( Comment, "-->" ), ( LineBreak, "\n" ), ( Normal, "</" ), ( Tag, "script" ) ])
        ]
