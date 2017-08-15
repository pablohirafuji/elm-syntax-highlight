module Language.Elm exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal)
import Test exposing (..)
import SyntaxHighlight.Language.Elm as Elm exposing (..)


suite : Test
suite =
    describe "Elm Language Test Suite"
        [ test "Module declaration" <|
            \() ->
                Elm.toSyntax moduleDeclarationText
                    |> equal (Ok moduleDeclarationResult)
        , test "Port module declaration" <|
            \() ->
                Elm.toSyntax ("port " ++ moduleDeclarationText)
                    |> equal (Ok ([ ( Keyword, "port" ), ( Space, " " ) ] ++ moduleDeclarationResult))
        , test "Import declaration" <|
            \() ->
                Elm.toSyntax "import Html.Attributes as Att exposing (Html, classList, (|>))"
                    |> equal (Ok [ ( Keyword, "import" ), ( Space, " " ), ( Normal, "Html.Attributes" ), ( Space, " " ), ( Keyword, "as" ), ( Space, " " ), ( Normal, "Att" ), ( Space, " " ), ( Keyword, "exposing" ), ( Space, " " ), ( Normal, "(" ), ( TypeSignature, "Html" ), ( Normal, "," ), ( Space, " " ), ( Function, "classList" ), ( Normal, "," ), ( Space, " " ), ( Infix, "(|>)" ), ( Normal, ")" ) ])
        , test "Function signature" <|
            \() ->
                Elm.toSyntax functionSignatureText
                    |> equal (Ok functionSignatureResult)
        , test "Port function signature" <|
            \() ->
                Elm.toSyntax ("port " ++ functionSignatureText)
                    |> equal (Ok ([ ( Keyword, "port" ), ( Space, " " ) ] ++ functionSignatureResult))
        , test "Function body" <|
            \() ->
                Elm.toSyntax "text str ="
                    |> equal (Ok [ ( Function, "text" ), ( Space, " " ), ( Normal, "str" ), ( Space, " " ), ( BasicSymbol, "=" ) ])
        , test "Case statement" <|
            \() ->
                Elm.toSyntax "    case maybe of\n        Just str -> str\n        Nothing -> str"
                    |> equal (Ok [ ( Space, "    " ), ( Keyword, "case" ), ( Space, " " ), ( Normal, "maybe" ), ( Space, " " ), ( Keyword, "of" ), ( LineBreak, "\n" ), ( Space, "        " ), ( Capitalized, "Just" ), ( Space, " " ), ( Normal, "str" ), ( Space, " " ), ( BasicSymbol, "->" ), ( Space, " " ), ( Normal, "str" ), ( LineBreak, "\n" ), ( Space, "        " ), ( Capitalized, "Nothing" ), ( Space, " " ), ( BasicSymbol, "->" ), ( Space, " " ), ( Normal, "str" ) ])
        , test "Numbers" <|
            \() ->
                Elm.toSyntax "math = (3+4.453) / 5 * 4.4"
                    |> equal (Ok [ ( Function, "math" ), ( Space, " " ), ( BasicSymbol, "=" ), ( Space, " " ), ( BasicSymbol, "(" ), ( Number, "3" ), ( BasicSymbol, "+" ), ( Number, "4.453" ), ( BasicSymbol, ")" ), ( Space, " " ), ( BasicSymbol, "/" ), ( Space, " " ), ( Number, "5" ), ( Space, " " ), ( BasicSymbol, "*" ), ( Space, " " ), ( Number, "4.4" ) ])
        , test "String literal: Single quote" <|
            \() ->
                Elm.toSyntax "char = 'c'"
                    |> equal (Ok [ ( Function, "char" ), ( Space, " " ), ( BasicSymbol, "=" ), ( Space, " " ), ( String, "'c'" ) ])
        , test "String literal: Double quote" <|
            \() ->
                Elm.toSyntax "string = \"hello\""
                    |> equal (Ok [ ( Function, "string" ), ( Space, " " ), ( BasicSymbol, "=" ), ( Space, " " ), ( String, "\"hello\"" ) ])
        , test "String literal: Triple double quote" <|
            \() ->
                Elm.toSyntax "string = \"\"\"Great\nString\" with \"\" double quotes\"\"\" finished"
                    |> equal (Ok [ ( Function, "string" ), ( Space, " " ), ( BasicSymbol, "=" ), ( Space, " " ), ( String, "\"\"\"Great\nString\" with \"\" double quotes\"\"\"" ), ( Space, " " ), ( Normal, "finished" ) ])
        , test "Comment: Inline" <|
            \() ->
                Elm.toSyntax "function = -- Comment\n    functionBody"
                    |> equal (Ok [ ( Function, "function" ), ( Space, " " ), ( BasicSymbol, "=" ), ( Space, " " ), ( Comment, "-- Comment" ), ( LineBreak, "\n" ), ( Space, "    " ), ( Normal, "functionBody" ) ])
        , test "Comment: Multiline" <|
            \() ->
                Elm.toSyntax "function = {- Multi\nline\ncomment-} functionBody"
                    |> equal (Ok [ ( Function, "function" ), ( Space, " " ), ( BasicSymbol, "=" ), ( Space, " " ), ( Comment, "{- Multi\nline\ncomment-}" ), ( Space, " " ), ( Normal, "functionBody" ) ])
        , test "Infix" <|
            \() ->
                Elm.toSyntax "(,)"
                    |> equal (Ok [ ( Infix, "(,)" ) ])
        ]


moduleDeclarationText : String
moduleDeclarationText =
    "module Main exposing (parser, Type)"


moduleDeclarationResult : List ( SyntaxType, String )
moduleDeclarationResult =
    [ ( Keyword, "module" ), ( Space, " " ), ( Normal, "Main" ), ( Space, " " ), ( Keyword, "exposing" ), ( Space, " " ), ( Normal, "(" ), ( Function, "parser" ), ( Normal, "," ), ( Space, " " ), ( TypeSignature, "Type" ), ( Normal, ")" ) ]


functionSignatureText : String
functionSignatureText =
    "text : (SyntaxType, String) -> Html msg"


functionSignatureResult : List ( SyntaxType, String )
functionSignatureResult =
    [ ( Function, "text" ), ( Space, " " ), ( BasicSymbol, ":" ), ( Space, " " ), ( Normal, "(" ), ( TypeSignature, "SyntaxType" ), ( Normal, "," ), ( Space, " " ), ( TypeSignature, "String" ), ( Normal, ")" ), ( Space, " " ), ( BasicSymbol, "->" ), ( Space, " " ), ( TypeSignature, "Html" ), ( Space, " " ), ( Normal, "msg" ) ]
