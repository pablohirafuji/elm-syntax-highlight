module Language.Javascript exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal, onFail)
import Fuzz exposing (string)
import Test exposing (..)
import Parser
import SyntaxHighlight.Language.Javascript as Javascript exposing (..)


suite : Test
suite =
    describe "Javascript Language Test Suite"
        [ equalTest "Function* declaration" "function* anotherGenerator(i) {\n  yield i + 1;\n  yield i + 2;\n  yield i + 3;\n}" <|
            Ok [ ( DeclarationKeyword, "function" ), ( Keyword, "*" ), ( Normal, " " ), ( Function, "anotherGenerator" ), ( Normal, "(" ), ( Param, "i" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Keyword, "yield" ), ( Normal, " " ), ( Normal, "i" ), ( Normal, " " ), ( Keyword, "+" ), ( Normal, " " ), ( LiteralKeyword, "1" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Keyword, "yield" ), ( Normal, " " ), ( Normal, "i" ), ( Normal, " " ), ( Keyword, "+" ), ( Normal, " " ), ( LiteralKeyword, "2" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Keyword, "yield" ), ( Normal, " " ), ( Normal, "i" ), ( Normal, " " ), ( Keyword, "+" ), ( Normal, " " ), ( LiteralKeyword, "3" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]
        , equalTest "Class declaration" "class Pessoa extends Humano {\n  constructor(nome, peso, altura) {\n   super(peso, altura);\n   this.nome = nome;\n  }\n}" <|
            Ok [ ( DeclarationKeyword, "class" ), ( Normal, " " ), ( Function, "Pessoa" ), ( Normal, " " ), ( Keyword, "extends" ), ( Normal, " " ), ( ClassExtends, "Humano" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Function, "constructor" ), ( Normal, "(" ), ( Param, "nome" ), ( Normal, "," ), ( Normal, " " ), ( Param, "peso" ), ( Normal, "," ), ( Normal, " " ), ( Param, "altura" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "   " ), ( Param, "super" ), ( Normal, "(" ), ( Normal, "peso" ), ( Normal, "," ), ( Normal, " " ), ( Normal, "altura" ), ( Normal, ");" ), ( LineBreak, "\n" ), ( Normal, "   " ), ( Param, "this" ), ( Keyword, "." ), ( Normal, "nome" ), ( Normal, " " ), ( Keyword, "=" ), ( Normal, " " ), ( Normal, "nome" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]
        , equalTest "Function" "function resolveAfter2Seconds(x) {\n  return new Promise(resolve => {\n    setTimeout(() => {\n      resolve(x);\n    }, 2000);\n  });\n};" <|
            Ok [ ( DeclarationKeyword, "function" ), ( Normal, " " ), ( Function, "resolveAfter2Seconds" ), ( Normal, "(" ), ( Param, "x" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Keyword, "return" ), ( Normal, " " ), ( Keyword, "new" ), ( Normal, " " ), ( FunctionEval, "Promise" ), ( Normal, "(" ), ( Normal, "resolve" ), ( Normal, " " ), ( Keyword, "=>" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( FunctionEval, "setTimeout" ), ( Normal, "(" ), ( Normal, "()" ), ( Normal, " " ), ( Keyword, "=>" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "      " ), ( FunctionEval, "resolve" ), ( Normal, "(" ), ( Normal, "x" ), ( Normal, ");" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}," ), ( Normal, " " ), ( LiteralKeyword, "2000" ), ( Normal, ");" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "});" ), ( LineBreak, "\n" ), ( Normal, "};" ) ]
        , equalTest "Inline Comment" "// Comment\nvar = id; //Comment\n// Comment" <|
            Ok [ ( Comment, "// Comment" ), ( LineBreak, "\n" ), ( DeclarationKeyword, "var" ), ( Normal, " " ), ( Keyword, "=" ), ( Normal, " " ), ( Normal, "id" ), ( Normal, ";" ), ( Normal, " " ), ( Comment, "//Comment" ), ( LineBreak, "\n" ), ( Comment, "// Comment" ) ]
        , equalTest "Multiline Comment" "/* Comment */\nvar = id; /* Comment */\n/* Comment" <|
            Ok [ ( Comment, "/*" ), ( Comment, " Comment " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( DeclarationKeyword, "var" ), ( Normal, " " ), ( Keyword, "=" ), ( Normal, " " ), ( Normal, "id" ), ( Normal, ";" ), ( Normal, " " ), ( Comment, "/*" ), ( Comment, " Comment " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( Comment, "/*" ), ( Comment, " Comment" ) ]
        , fuzz string "The result should always be Ok" <|
            \fuzzStr ->
                Javascript.toSyntax fuzzStr
                    |> Result.map (always [])
                    |> equal (Ok [])
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]


equalTest : String -> String -> Result Parser.Error (List ( SyntaxType, String )) -> Test
equalTest testName testStr testResult =
    test testName <|
        \() ->
            Javascript.toSyntax testStr
                |> Result.map List.reverse
                |> equal testResult
