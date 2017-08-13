module SyntaxHighlight.Language.Javascript exposing (parse)

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen)
import SyntaxHighlight.Style exposing (Style, Color(..), normal, emphasis)
import SyntaxHighlight.Helpers exposing (isWhitespace, isSpace, isLineBreak, delimited)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | String
    | Keyword
    | DeclarationKeyword
    | FunctionEval
    | Function
    | LiteralKeyword
    | Param


parse : String -> Result Error (List ( Style, String ))
parse =
    Parser.run (mainLoop [])
        >> Result.map (List.map (Tuple.mapFirst syntaxToStyle))


mainLoop : List Syntax -> Parser (List Syntax)
mainLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , stringLiteral
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , whitespace
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , keep oneOrMore isOperatorChar
            |> andThen (\n -> mainLoop (( Keyword, n ) :: revSyntaxList))
        , keep oneOrMore isGroupChar
            |> andThen
                (\n -> mainLoop (( Normal, n ) :: revSyntaxList))
        , keep oneOrMore isIdentifierNameChar
            |> andThen
                (\n ->
                    if n == "function" || n == "static" then
                        functionStatementLoop
                            (( DeclarationKeyword, n ) :: revSyntaxList)
                    else if n == "class" then
                        classStatementLoop
                            (( DeclarationKeyword, n ) :: revSyntaxList)
                    else if n == "this" || n == "super" then
                        mainLoop (( Param, n ) :: revSyntaxList)
                    else if n == "constructor" then
                        functionStatementLoop (( Function, n ) :: revSyntaxList)
                    else if isReservedWord1 n then
                        mainLoop (( Keyword, n ) :: revSyntaxList)
                    else if isReservedWord2 n then
                        mainLoop (( DeclarationKeyword, n ) :: revSyntaxList)
                    else if isLiteral n then
                        mainLoop (( LiteralKeyword, n ) :: revSyntaxList)
                    else
                        functionEvalLoop n [] revSyntaxList
                )
        , end revSyntaxList
        ]


functionStatementLoop : List Syntax -> Parser (List Syntax)
functionStatementLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> functionStatementLoop (n :: revSyntaxList))
        , whitespace
            |> andThen (\n -> functionStatementLoop (n :: revSyntaxList))
        , keep oneOrMore isIdentifierNameChar
            |> andThen (\n -> functionStatementLoop (( Function, n ) :: revSyntaxList))
        , symbol "("
            |> andThen (\n -> argLoop (( Normal, "(" ) :: revSyntaxList))
        , mainLoop revSyntaxList
        ]


functionEvalLoop : String -> List Syntax -> List Syntax -> Parser (List Syntax)
functionEvalLoop identifier postSyntaxList preSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> functionEvalLoop identifier (n :: postSyntaxList) preSyntaxList)
        , whitespace
            |> andThen (\n -> functionEvalLoop identifier (n :: postSyntaxList) preSyntaxList)
        , symbol "("
            |> andThen
                (\n ->
                    mainLoop
                        ((( Normal, "(" ) :: postSyntaxList)
                            ++ (( FunctionEval, identifier )
                                    :: preSyntaxList
                               )
                        )
                )
        , mainLoop (postSyntaxList ++ (( Normal, identifier ) :: preSyntaxList))
        ]


classStatementLoop : List Syntax -> Parser (List Syntax)
classStatementLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> classStatementLoop (n :: revSyntaxList))
        , whitespace
            |> andThen (\n -> classStatementLoop (n :: revSyntaxList))
        , keep oneOrMore isIdentifierNameChar
            |> andThen (\n -> mainLoop (( Function, n ) :: revSyntaxList))
        , mainLoop revSyntaxList
        ]


argLoop : List Syntax -> Parser (List Syntax)
argLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> argLoop (n :: revSyntaxList))
        , whitespace
            |> andThen (\n -> argLoop (n :: revSyntaxList))
        , keep oneOrMore (\c -> not (isCommentChar c || isWhitespace c || c == ',' || c == ')'))
            |> andThen (\n -> argLoop (( Param, n ) :: revSyntaxList))
        , keep oneOrMore (\c -> c == '/' || c == ',')
            |> andThen (\n -> argLoop (( Normal, n ) :: revSyntaxList))
        , symbol ")"
            |> andThen (\n -> mainLoop (( Normal, ")" ) :: revSyntaxList))
        , end revSyntaxList
        ]


isIdentifierNameChar : Char -> Bool
isIdentifierNameChar c =
    not
        (isPunctuaction c
            || isStringLiteralChar c
            || isCommentChar c
            || isWhitespace c
        )



-- Reserved Words


isReservedWord1 : String -> Bool
isReservedWord1 str =
    Set.member str reservedWord1Set


isReservedWord2 : String -> Bool
isReservedWord2 str =
    Set.member str reservedWord2Set


reservedWord1Set : Set String
reservedWord1Set =
    [ "break"
    , "do"
    , "instanceof"
    , "typeof"
    , "case"
    , "else"
    , "new"
    , "catch"
    , "finally"
    , "return"
    , "void"
    , "continue"
    , "for"
    , "switch"
    , "while"
    , "debugger"
    , "this"
    , "with"
    , "default"
    , "if"
    , "throw"
    , "delete"
    , "in"
    , "try"
    , "enum"
    , "extends"
    , "export"
    , "import"
    , "implements"
    , "private"
    , "public"
    , "yield"
    , "interface"
    , "package"
    , "protected"
    ]
        |> Set.fromList


reservedWord2Set : Set String
reservedWord2Set =
    [ "var"
    , "const"
    , "let"
    ]
        |> Set.fromList


punctuactorSet : Set Char
punctuactorSet =
    Set.union operatorSet groupSet


isPunctuaction : Char -> Bool
isPunctuaction c =
    Set.member c punctuactorSet


operatorSet : Set Char
operatorSet =
    Set.fromList
        [ '+'
        , '-'
        , '*'
        , '/'
        , '='
        , '!'
        , '<'
        , '>'
        , '&'
        , '|'
        , '?'
        , '^'
        , ':'
        , '~'
        , '%'
        , '.'
        ]


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c operatorSet


groupSet : Set Char
groupSet =
    Set.fromList
        [ '{'
        , '}'
        , '('
        , ')'
        , '['
        , ']'
        , ','
        , ';'
        ]


isGroupChar : Char -> Bool
isGroupChar c =
    Set.member c groupSet


literalSet : Set String
literalSet =
    [ "true"
    , "false"
    , "null"
    , "undefined"
    , "NaN"
    , "Infinity"
    ]
        |> Set.fromList


isLiteral : String -> Bool
isLiteral str =
    Set.member str literalSet


stringLiteral : Parser Syntax
stringLiteral =
    oneOf
        [ templateString
        , doubleQuote
        , singleQuote
        ]
        |> source
        |> map ((,) String)


templateString : Parser ()
templateString =
    delimited
        { start = "`"
        , end = "`"
        , isNestable = False
        , isEscapable = True
        }


doubleQuote : Parser ()
doubleQuote =
    delimited
        { start = "\""
        , end = "\""
        , isNestable = False
        , isEscapable = True
        }


singleQuote : Parser ()
singleQuote =
    delimited
        { start = "'"
        , end = "'"
        , isNestable = False
        , isEscapable = True
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\'' || c == '`'



-- Comments


comment : Parser Syntax
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]
        |> source
        |> map ((,) Comment)


inlineComment : Parser ()
inlineComment =
    symbol "//"
        |. ignore zeroOrMore (not << isLineBreak)


multilineComment : Parser ()
multilineComment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , isEscapable = False
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '/'


whitespace : Parser Syntax
whitespace =
    keep oneOrMore isWhitespace
        |> map ((,) Normal)


end : List Syntax -> Parser (List Syntax)
end revSyntaxList =
    Parser.end
        |> map (\_ -> List.reverse revSyntaxList)


syntaxToStyle : SyntaxType -> Style
syntaxToStyle syntaxType =
    case syntaxType of
        Normal ->
            normal Default

        Comment ->
            normal Color1

        String ->
            normal Color2

        Keyword ->
            normal Color3

        DeclarationKeyword ->
            emphasis Color4

        FunctionEval ->
            normal Color4

        Function ->
            normal Color5

        LiteralKeyword ->
            normal Color6

        Param ->
            normal Color7
