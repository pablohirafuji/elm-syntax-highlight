module Language.Elm exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Elm as Elm exposing (Syntax(..), toRevTokens)
import SyntaxHighlight.Language.Type as T exposing (Syntax(..))
import Test exposing (..)


suite : Test
suite =
    describe "Elm Language Test Suite"
        [ equalTest "Module declaration" moduleDeclarationText moduleDeclarationResult
        , equalTest "Port module declaration" ("port " ++ moduleDeclarationText) ([ ( T.C Keyword, "port" ), ( Normal, " " ) ] ++ moduleDeclarationResult)
        , equalTest "Import declaration" "import Html.Attributes as Att exposing (Html, classList, (|>))" [ ( C Keyword, "import" ), ( Normal, " " ), ( Normal, "Html.Attributes" ), ( Normal, " " ), ( C Keyword, "as" ), ( Normal, " " ), ( Normal, "Att" ), ( Normal, " " ), ( T.C Keyword, "exposing" ), ( Normal, " " ), ( Normal, "(" ), ( C TypeSignature, "Html" ), ( Normal, "," ), ( Normal, " " ), ( C Function, "classList" ), ( Normal, "," ), ( Normal, " " ), ( C Function, "(|>)" ), ( Normal, ")" ) ]
        , equalTest "Function signature" functionSignatureText functionSignatureResult
        , equalTest "Port function signature" ("port " ++ functionSignatureText) ([ ( T.C Keyword, "port" ), ( Normal, " " ) ] ++ functionSignatureResult)
        , equalTest "Function body" "text str =" [ ( C Function, "text" ), ( Normal, " " ), ( Normal, "str" ), ( Normal, " " ), ( C BasicSymbol, "=" ) ]
        , equalTest "Case statement" "    case maybe of\n        Just str -> str\n        Nothing -> str" [ ( Normal, "    " ), ( T.C Keyword, "case" ), ( Normal, " " ), ( Normal, "maybe" ), ( Normal, " " ), ( T.C Keyword, "of" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C Capitalized, "Just" ), ( Normal, " " ), ( Normal, "str" ), ( Normal, " " ), ( C BasicSymbol, "->" ), ( Normal, " " ), ( Normal, "str" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C Capitalized, "Nothing" ), ( Normal, " " ), ( C BasicSymbol, "->" ), ( Normal, " " ), ( Normal, "str" ) ]
        , equalTest "Numbers" "math = (3+4.453) / 5 * 4.4" [ ( C Function, "math" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C BasicSymbol, "(" ), ( C Number, "3" ), ( C BasicSymbol, "+" ), ( C Number, "4.453" ), ( C BasicSymbol, ")" ), ( Normal, " " ), ( C BasicSymbol, "/" ), ( Normal, " " ), ( C Number, "5" ), ( Normal, " " ), ( C BasicSymbol, "*" ), ( Normal, " " ), ( C Number, "4.4" ) ]
        , equalTest "String literal: Single quote" "char = 'c'" [ ( C Function, "char" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C String, "'" ), ( C String, "c" ), ( C String, "'" ) ]
        , equalTest "String literal: Double quote" "string = \"hello\"" [ ( C Function, "string" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "hello" ), ( C String, "\"" ) ]
        , equalTest "String literal: Triple double quote" "string = \"\"\"Great\nString\" with \"\" double quotes\"\"\" finished" [ ( C Function, "string" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( C String, "\"\"\"" ), ( C String, "Great" ), ( LineBreak, "\n" ), ( C String, "String" ), ( C String, "\" with " ), ( C String, "\"" ), ( C String, "\" double quotes" ), ( C String, "\"\"\"" ), ( Normal, " " ), ( Normal, "finished" ) ]
        , equalTest "Comment: Inline" "function = -- Comment\n    functionBody" [ ( C Function, "function" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( Comment, "-- Comment" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "functionBody" ) ]
        , equalTest "Comment: Multiline" "function = {- Multi\nline\ncomment-} functionBody" [ ( C Function, "function" ), ( Normal, " " ), ( C BasicSymbol, "=" ), ( Normal, " " ), ( Comment, "{-" ), ( Comment, " Multi" ), ( LineBreak, "\n" ), ( Comment, "line" ), ( LineBreak, "\n" ), ( Comment, "comment" ), ( Comment, "-}" ), ( Normal, " " ), ( Normal, "functionBody" ) ]
        , equalTest "Infix" "(,)" [ ( C Function, "(,)" ) ]
        , fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run Elm.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]


equalTest : String -> String -> List ( T.Syntax Elm.Syntax, String ) -> Test
equalTest testName testStr testExpec =
    describe testName
        [ test "Syntax equality" <|
            \() ->
                Parser.run Elm.toRevTokens testStr
                    |> Result.map List.reverse
                    |> Result.withDefault []
                    |> equalLists testExpec
        , test "String equality" <|
            \() ->
                Parser.run Elm.toRevTokens testStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok testStr)
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
