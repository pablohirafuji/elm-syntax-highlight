module Language.Javascript exposing (suite)

import Expect exposing (Expectation, equal, equalLists, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Javascript as JS exposing (Syntax(..), toRevTokens)
import SyntaxHighlight.Language.Type as T exposing (Syntax(..))
import Test exposing (..)


suite : Test
suite =
    describe "Javascript Language Test Suite"
        [ equalTest "Function* declaration"
            "function* anotherGenerator(i) {\n  yield i + 1;\n  yield i + 2;\n  yield i + 3;\n}"
          <|
            Ok [ ( C DeclarationKeyword, "function" ), ( C Keyword, "*" ), ( Normal, " " ), ( C Function, "anotherGenerator" ), ( Normal, "(" ), ( C Param, "i" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( C Keyword, "yield" ), ( Normal, " " ), ( Normal, "i" ), ( Normal, " " ), ( C Keyword, "+" ), ( Normal, " " ), ( C Number, "1" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( C Keyword, "yield" ), ( Normal, " " ), ( Normal, "i" ), ( Normal, " " ), ( C Keyword, "+" ), ( Normal, " " ), ( C Number, "2" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( C Keyword, "yield" ), ( Normal, " " ), ( Normal, "i" ), ( Normal, " " ), ( C Keyword, "+" ), ( Normal, " " ), ( C Number, "3" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]
        , equalTest "Class declaration"
            "class Pessoa extends Humano {\n  constructor(nome, peso, altura) {\n   super(peso, altura);\n   this.nome = nome;\n  }\n}"
          <|
            Ok [ ( C DeclarationKeyword, "class" ), ( Normal, " " ), ( C Function, "Pessoa" ), ( Normal, " " ), ( C Keyword, "extends" ), ( Normal, " " ), ( C ClassExtends, "Humano" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( C Function, "constructor" ), ( Normal, "(" ), ( C Param, "nome" ), ( Normal, "," ), ( Normal, " " ), ( C Param, "peso" ), ( Normal, "," ), ( Normal, " " ), ( C Param, "altura" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "   " ), ( C Param, "super" ), ( Normal, "(" ), ( Normal, "peso" ), ( Normal, "," ), ( Normal, " " ), ( Normal, "altura" ), ( Normal, ");" ), ( LineBreak, "\n" ), ( Normal, "   " ), ( C Param, "this" ), ( C Keyword, "." ), ( Normal, "nome" ), ( Normal, " " ), ( C Keyword, "=" ), ( Normal, " " ), ( Normal, "nome" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]
        , equalTest "Function"
            "function resolveAfter2Seconds(x) {\n  return new Promise(resolve => {\n    setTimeout(() => {\n      resolve(x);\n    }, 2000);\n  });\n};"
          <|
            Ok [ ( C DeclarationKeyword, "function" ), ( Normal, " " ), ( C Function, "resolveAfter2Seconds" ), ( Normal, "(" ), ( C Param, "x" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( C Keyword, "return" ), ( Normal, " " ), ( C Keyword, "new" ), ( Normal, " " ), ( C FunctionEval, "Promise" ), ( Normal, "(" ), ( Normal, "resolve" ), ( Normal, " " ), ( C Keyword, "=>" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C FunctionEval, "setTimeout" ), ( Normal, "(" ), ( Normal, "()" ), ( Normal, " " ), ( C Keyword, "=>" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "      " ), ( C FunctionEval, "resolve" ), ( Normal, "(" ), ( Normal, "x" ), ( Normal, ");" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}," ), ( Normal, " " ), ( C Number, "2000" ), ( Normal, ");" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "});" ), ( LineBreak, "\n" ), ( Normal, "};" ) ]
        , equalTest "Inline Comment" "// Comment\nvar = id; //Comment\n// Comment" <|
            Ok [ ( Comment, "// Comment" ), ( LineBreak, "\n" ), ( C DeclarationKeyword, "var" ), ( Normal, " " ), ( C Keyword, "=" ), ( Normal, " " ), ( Normal, "id" ), ( Normal, ";" ), ( Normal, " " ), ( Comment, "//Comment" ), ( LineBreak, "\n" ), ( Comment, "// Comment" ) ]
        , equalTest "Multiline Comment" "/* Comment */\nvar = id; /* Comment */\n/* Comment" <|
            Ok [ ( Comment, "/*" ), ( Comment, " Comment " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( C DeclarationKeyword, "var" ), ( Normal, " " ), ( C Keyword, "=" ), ( Normal, " " ), ( Normal, "id" ), ( Normal, ";" ), ( Normal, " " ), ( Comment, "/*" ), ( Comment, " Comment " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( Comment, "/*" ), ( Comment, " Comment" ) ]
        , fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run JS.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]


equalTest : String -> String -> Result (List Parser.DeadEnd) (List ( T.Syntax JS.Syntax, String )) -> Test
equalTest testName testStr testResult =
    describe testName
        [ test "Syntax equality" <|
            \() ->
                Parser.run JS.toRevTokens testStr
                    |> Result.map List.reverse
                    |> equal testResult
        , test "String equality" <|
            \() ->
                Parser.run JS.toRevTokens testStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok testStr)
        ]
