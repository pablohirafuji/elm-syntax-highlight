module Language.Elm exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal, onFail)
import Fuzz exposing (string)
import Test exposing (..)
import SyntaxHighlight.Language.Type as T exposing (Syntax(..))
import SyntaxHighlight.Language.Elm as Elm exposing (Syntax(..), toRevTokens)


suite : Test
suite =
    describe "Elm Language Test Suite"
        [ test "Module declaration" <|
            \() ->
                Elm.toRevTokens moduleDeclarationText
                    |> Result.map List.reverse
                    |> equal (Ok moduleDeclarationResult)
        , test "Port module declaration" <|
            \() ->
                Elm.toRevTokens ("port " ++ moduleDeclarationText)
                    |> Result.map List.reverse
                    |> equal (Ok ([ ( T.C Keyword, "port" ), ( Normal, " " ) ] ++ moduleDeclarationResult))
        , test "Import declaration" <|
            \() ->
                Elm.toRevTokens "import Html.Attributes as Att exposing (Html, classList, (|>))"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Keyword, "import" ), ( Normal, " " ), ( Normal, "Html.Attributes" ), ( Normal, " " ), ( C Keyword, "as" ), ( Normal, " " ), ( Normal, "Att" ), ( Normal, " " ), ( T.C Keyword, "exposing" ), ( Normal, " " ), ( Normal, "(" ), ( C TypeSignature, "Html" ), ( Normal, "," ), ( Normal, " " ), ( C Function, "classList" ), ( Normal, "," ), ( Normal, " " ), ( C Function, "(|>)" ), ( Normal, ")" ) ])
        , test "Function signature" <|
            \() ->
                Elm.toRevTokens functionSignatureText
                    |> Result.map List.reverse
                    |> equal (Ok functionSignatureResult)
        , test "Port function signature" <|
            \() ->
                Elm.toRevTokens ("port " ++ functionSignatureText)
                    |> Result.map List.reverse
                    |> equal (Ok ([ ( T.C Keyword, "port" ), ( Normal, " " ) ] ++ functionSignatureResult))
        , test "Function body" <|
            \() ->
                Elm.toRevTokens "text str ="
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "text" ), ( Normal, " " ), ( Normal, "str" ), ( Normal, " " ), ( C BasicSymbol, "=" ) ])
        , test "Case statement" <|
            \() ->
                Elm.toRevTokens "    case maybe of\n        Just str -> str\n        Nothing -> str"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( Normal, "    " ), ( T.C Keyword, "case" ), ( Normal, " " ), ( Normal, "maybe" ), ( Normal, " " ), ( T.C Keyword, "of" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C Capitalized, "Just" ), ( Normal, " " ), ( Normal, "str" ), ( Normal, " " ), ( C BasicSymbol, "->" ), ( Normal, " " ), ( Normal, "str" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C Capitalized, "Nothing" ), ( Normal, " " ), ( C BasicSymbol, "->" ), ( Normal, " " ), ( Normal, "str" ) ])
        , test "Numbers" <|
            \() ->
                Elm.toRevTokens "math = (3+4.453) / 5 * 4.4"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "math" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C BasicSymbol, "(" ), ( C Number, "3" ), ( C BasicSymbol, "+" ), ( C Number, "4.453" ), ( C BasicSymbol, ")" ), ( Normal, " " ), ( C BasicSymbol, "/" ), ( Normal, " " ), ( C Number, "5" ), ( Normal, " " ), ( C BasicSymbol, "*" ), ( Normal, " " ), ( C Number, "4.4" ) ])
        , test "String literal: Single quote" <|
            \() ->
                Elm.toRevTokens "char = 'c'"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "char" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C String, "'" ), ( C String, "c" ), ( C String, "'" ) ])
        , test "String literal: Double quote" <|
            \() ->
                Elm.toRevTokens "string = \"hello\""
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "string" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "hello" ), ( C String, "\"" ) ])
        , test "String literal: Triple double quote" <|
            \() ->
                Elm.toRevTokens "string = \"\"\"Great\nString\" with \"\" double quotes\"\"\" finished"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "string" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C String, "\"\"\"" ), ( C String, "Great" ), ( LineBreak, "\n" ), ( C String, "String" ), ( C String, "\" with " ), ( C String, "\"" ), ( C String, "\" double quotes" ), ( C String, "\"\"\"" ), ( Normal, " " ), ( Normal, "finished" ) ])
        , test "Comment: Inline" <|
            \() ->
                Elm.toRevTokens "function = -- Comment\n    functionBody"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "function" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( Comment, "-- Comment" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "functionBody" ) ])
        , test "Comment: Multiline" <|
            \() ->
                Elm.toRevTokens "function = {- Multi\nline\ncomment-} functionBody"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "function" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( Comment, "{-" ), ( Comment, " Multi" ), ( LineBreak, "\n" ), ( Comment, "line" ), ( LineBreak, "\n" ), ( Comment, "comment" ), ( Comment, "-}" ), ( Normal, " " ), ( Normal, "functionBody" ) ])
        , test "Infix" <|
            \() ->
                Elm.toRevTokens "(,)"
                    |> Result.map List.reverse
                    |> equal (Ok [ ( C Function, "(,)" ) ])
        , fuzz string "The result should always be Ok" <|
            \fuzzStr ->
                Elm.toRevTokens fuzzStr
                    |> Result.map (always [])
                    |> equal (Ok [])
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]


moduleDeclarationText : String
moduleDeclarationText =
    "module Main exposing (parser, Type)"


moduleDeclarationResult : List ( T.Syntax Elm.Syntax, String )
moduleDeclarationResult =
    [ ( T.C Keyword, "module" ), ( Normal, " " ), ( Normal, "Main" ), ( Normal, " " ), ( T.C Keyword, "exposing" ), ( Normal, " " ), ( Normal, "(" ), ( C Function, "parser" ), ( Normal, "," ), ( Normal, " " ), ( T.C TypeSignature, "Type" ), ( Normal, ")" ) ]


functionSignatureText : String
functionSignatureText =
    "text : (SyntaxType, String) -> Html msg"


functionSignatureResult : List ( T.Syntax Elm.Syntax, String )
functionSignatureResult =
    [ ( C Function, "text" ), ( Normal, " " ), ( C BasicSymbol, ":" ), ( Normal, " " ), ( Normal, "(" ), ( T.C TypeSignature, "SyntaxType" ), ( Normal, "," ), ( Normal, " " ), ( T.C TypeSignature, "String" ), ( Normal, ")" ), ( Normal, " " ), ( C BasicSymbol, "->" ), ( Normal, " " ), ( T.C TypeSignature, "Html" ), ( Normal, " " ), ( Normal, "msg" ) ]
