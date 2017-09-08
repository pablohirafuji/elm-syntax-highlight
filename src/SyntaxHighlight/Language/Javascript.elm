module SyntaxHighlight.Language.Javascript
    exposing
        ( parse
          -- Exposing just for tests purpose:
        , toSyntax
        , SyntaxType(..)
        )

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed)
import SyntaxHighlight.Line exposing (Line, Fragment, Color(..))
import SyntaxHighlight.Line.Helpers exposing (toLines, normal, emphasis, strong)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited, escapable, isEscapable, addThen, consThen)


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
    | LineBreak


parse : String -> Result Error (List Line)
parse =
    toSyntax
        >> Result.map (toLines LineBreak toFragment)


toSyntax : String -> Result Error (List Syntax)
toSyntax =
    mainLoop
        |> repeat zeroOrMore
        |> map (List.reverse >> List.concat)
        |> Parser.run


mainLoop : Parser (List Syntax)
mainLoop =
    oneOf
        [ whitespaceOrComment
        , stringLiteral
        , oneOf
            [ operatorChar
            , groupChar
            , number
            ]
            |> map List.singleton
        , keep oneOrMore isIdentifierNameChar
            |> andThen keywordParser
        ]


keywordParser : String -> Parser (List Syntax)
keywordParser n =
    if n == "function" || n == "static" then
        functionDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( DeclarationKeyword, n ) ]
    else if n == "class" then
        classStatementLoop [ ( DeclarationKeyword, n ) ]
    else if n == "this" || n == "super" then
        succeed [ ( Param, n ) ]
    else if n == "constructor" then
        functionDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( Function, n ) ]
    else if isKeyword n then
        succeed [ ( Keyword, n ) ]
    else if isDeclarationKeyword n then
        succeed [ ( DeclarationKeyword, n ) ]
    else if isLiteralKeyword n then
        succeed [ ( LiteralKeyword, n ) ]
    else
        functionEvalLoop n [] []


functionDeclarationLoop : Parser (List Syntax)
functionDeclarationLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isIdentifierNameChar
            |> map ((,) Function >> List.singleton)
        , symbol "("
            |> andThen
                (\_ ->
                    argLoop
                        |> repeat zeroOrMore
                        |> consThenRevConcat [ ( Normal, "(" ) ]
                )
        ]


argLoop : Parser (List Syntax)
argLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore (\c -> not (isCommentChar c || isWhitespace c || c == ',' || c == ')'))
            |> map ((,) Param >> List.singleton)
        , keep oneOrMore (\c -> c == '/' || c == ',')
            |> map ((,) Normal >> List.singleton)
        ]


functionEvalLoop : String -> List Syntax -> List Syntax -> Parser (List Syntax)
functionEvalLoop identifier preSyntaxList postSyntaxList =
    oneOf
        [ whitespaceOrComment
            |> addThen (functionEvalLoop identifier preSyntaxList) postSyntaxList
        , symbol "("
            |> andThen
                (\n ->
                    succeed
                        ((( Normal, "(" ) :: postSyntaxList)
                            ++ (( FunctionEval, identifier )
                                    :: preSyntaxList
                               )
                        )
                )
        , succeed (postSyntaxList ++ (( Normal, identifier ) :: preSyntaxList))
        ]


classStatementLoop : List Syntax -> Parser (List Syntax)
classStatementLoop revSyntaxes =
    oneOf
        [ whitespaceOrComment
            |> addThen classStatementLoop revSyntaxes
        , keep oneOrMore isIdentifierNameChar
            |> map ((,) Function)
            |> consThen succeed revSyntaxes
        , succeed revSyntaxes
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


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
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


isDeclarationKeyword : String -> Bool
isDeclarationKeyword str =
    Set.member str declarationKeywordSet


declarationKeywordSet : Set String
declarationKeywordSet =
    Set.fromList
        [ "var"
        , "const"
        , "let"
        ]


isPunctuaction : Char -> Bool
isPunctuaction c =
    Set.member c punctuactorSet


punctuactorSet : Set Char
punctuactorSet =
    Set.union operatorSet groupSet


operatorChar : Parser Syntax
operatorChar =
    keep oneOrMore isOperatorChar
        |> map ((,) Keyword)


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c operatorSet


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


groupChar : Parser Syntax
groupChar =
    keep oneOrMore isGroupChar
        |> map ((,) Normal)


isGroupChar : Char -> Bool
isGroupChar c =
    Set.member c groupSet


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


isLiteralKeyword : String -> Bool
isLiteralKeyword str =
    Set.member str literalKeywordSet


literalKeywordSet : Set String
literalKeywordSet =
    Set.fromList
        [ "true"
        , "false"
        , "null"
        , "undefined"
        , "NaN"
        , "Infinity"
        ]



-- String literal


stringLiteral : Parser (List Syntax)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
        , templateString
        ]


quote : Parser (List Syntax)
quote =
    delimited quoteDelimiter


quoteDelimiter : Delimiter Syntax
quoteDelimiter =
    { start = "'"
    , end = "'"
    , isNestable = False
    , defaultMap = ((,) String)
    , innerParsers = [ lineBreakList, jsEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Syntax)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


templateString : Parser (List Syntax)
templateString =
    delimited
        { quoteDelimiter
            | start = "`"
            , end = "`"
            , innerParsers = [ lineBreakList, jsEscapable ]
            , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\'' || c == '`'



-- Comments


comment : Parser (List Syntax)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Syntax)
inlineComment =
    symbol "//"
        |. ignore zeroOrMore (not << isLineBreak)
        |> source
        |> map ((,) Comment >> List.singleton)


multilineComment : Parser (List Syntax)
multilineComment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = ((,) Comment)
        , innerParsers = [ lineBreakList ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '/'



-- Helpers


whitespaceOrComment : Parser (List Syntax)
whitespaceOrComment =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) Normal >> List.singleton)
        , lineBreakList
        , comment
        ]


lineBreakList : Parser (List Syntax)
lineBreakList =
    keep (Exactly 1) isLineBreak
        |> map ((,) LineBreak)
        |> repeat oneOrMore


number : Parser Syntax
number =
    SyntaxHighlight.Helpers.number
        |> source
        |> map ((,) LiteralKeyword)


jsEscapable : Parser (List Syntax)
jsEscapable =
    escapable
        |> source
        |> map ((,) LiteralKeyword)
        |> repeat oneOrMore


consThenRevConcat : List Syntax -> Parser (List (List Syntax)) -> Parser (List Syntax)
consThenRevConcat toCons =
    map ((::) toCons >> List.reverse >> List.concat)


toFragment : Syntax -> Fragment
toFragment ( syntaxType, text ) =
    case syntaxType of
        Normal ->
            normal Default text

        Comment ->
            normal Color1 text

        String ->
            normal Color2 text

        Keyword ->
            normal Color3 text

        DeclarationKeyword ->
            emphasis Color4 text

        FunctionEval ->
            normal Color4 text

        Function ->
            normal Color5 text

        LiteralKeyword ->
            normal Color6 text

        Param ->
            normal Color7 text

        LineBreak ->
            normal Default text
