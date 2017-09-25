module SyntaxHighlight.Language.Javascript
    exposing
        ( toLines
        , Syntax(..)
        , syntaxToStyle
          -- Exposing for tests purpose
        , toRevTokens
        )

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Language.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited, escapable, isEscapable, addThen, consThen)
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = Number
    | String
    | Keyword
    | DeclarationKeyword
    | FunctionEval
    | Function
    | LiteralKeyword
    | Param
    | ClassExtends


toLines : String -> Result Error (List Line)
toLines =
    toRevTokens
        >> Result.map (Line.toLines syntaxToStyle)


toRevTokens : String -> Result Error (List Token)
toRevTokens =
    mainLoop
        |> repeat zeroOrMore
        |> map (List.reverse >> List.concat)
        |> Parser.run


mainLoop : Parser (List Token)
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


keywordParser : String -> Parser (List Token)
keywordParser n =
    if n == "function" || n == "static" then
        functionDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( T.C DeclarationKeyword, n ) ]
    else if n == "class" then
        classDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( T.C DeclarationKeyword, n ) ]
    else if n == "this" || n == "super" then
        succeed [ ( T.C Param, n ) ]
    else if n == "constructor" then
        functionDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( T.C Function, n ) ]
    else if isKeyword n then
        succeed [ ( T.C Keyword, n ) ]
    else if isDeclarationKeyword n then
        succeed [ ( T.C DeclarationKeyword, n ) ]
    else if isLiteralKeyword n then
        succeed [ ( T.C LiteralKeyword, n ) ]
    else
        functionEvalLoop n []


functionDeclarationLoop : Parser (List Token)
functionDeclarationLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isIdentifierNameChar
            |> map ((,) (T.C Function) >> List.singleton)
        , symbol "*"
            |> map (\_ -> [ ( T.C Keyword, "*" ) ])
        , symbol "("
            |> andThen
                (\_ ->
                    argLoop
                        |> repeat zeroOrMore
                        |> consThenRevConcat [ ( T.Normal, "(" ) ]
                )
        ]


argLoop : Parser (List Token)
argLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore (\c -> not (isCommentChar c || isWhitespace c || c == ',' || c == ')'))
            |> map ((,) (T.C Param) >> List.singleton)
        , keep oneOrMore (\c -> c == '/' || c == ',')
            |> map ((,) T.Normal >> List.singleton)
        ]


functionEvalLoop : String -> List Token -> Parser (List Token)
functionEvalLoop identifier revTokens =
    oneOf
        [ whitespaceOrComment
            |> addThen (functionEvalLoop identifier) revTokens
        , symbol "("
            |> andThen
                (\n ->
                    succeed
                        ((( T.Normal, "(" ) :: revTokens)
                            ++ [ ( T.C FunctionEval, identifier ) ]
                        )
                )
        , succeed (revTokens ++ [ ( T.Normal, identifier ) ])
        ]


classDeclarationLoop : Parser (List Token)
classDeclarationLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isIdentifierNameChar
            |> andThen
                (\n ->
                    if n == "extends" then
                        classExtendsLoop
                            |> repeat zeroOrMore
                            |> consThenRevConcat [ ( T.C Keyword, n ) ]
                    else
                        succeed [ ( T.C Function, n ) ]
                )
        ]


classExtendsLoop : Parser (List Token)
classExtendsLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isIdentifierNameChar
            |> map ((,) (T.C ClassExtends) >> List.singleton)
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


operatorChar : Parser Token
operatorChar =
    keep oneOrMore isOperatorChar
        |> map ((,) (T.C Keyword))


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


groupChar : Parser Token
groupChar =
    keep oneOrMore isGroupChar
        |> map ((,) T.Normal)


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


stringLiteral : Parser (List Token)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
        , templateString
        ]


quote : Parser (List Token)
quote =
    delimited quoteDelimiter


quoteDelimiter : Delimiter Token
quoteDelimiter =
    { start = "'"
    , end = "'"
    , isNestable = False
    , defaultMap = ((,) (T.C String))
    , innerParsers = [ lineBreakList, jsEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Token)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


templateString : Parser (List Token)
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


comment : Parser (List Token)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Token)
inlineComment =
    symbol "//"
        |. ignore zeroOrMore (not << isLineBreak)
        |> source
        |> map ((,) T.Comment >> List.singleton)


multilineComment : Parser (List Token)
multilineComment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = ((,) T.Comment)
        , innerParsers = [ lineBreakList ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '/'



-- Helpers


whitespaceOrComment : Parser (List Token)
whitespaceOrComment =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) T.Normal >> List.singleton)
        , lineBreakList
        , comment
        ]


lineBreakList : Parser (List Token)
lineBreakList =
    keep (Exactly 1) isLineBreak
        |> map ((,) T.LineBreak)
        |> repeat oneOrMore


number : Parser Token
number =
    SyntaxHighlight.Language.Helpers.number
        |> source
        |> map ((,) (T.C Number))


jsEscapable : Parser (List Token)
jsEscapable =
    escapable
        |> source
        |> map ((,) (T.C LiteralKeyword))
        |> repeat oneOrMore


consThenRevConcat : List Token -> Parser (List (List Token)) -> Parser (List Token)
consThenRevConcat toCons =
    map ((::) toCons >> List.reverse >> List.concat)


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Number ->
            ( Style1, "js-n" )

        String ->
            ( Style2, "js-s" )

        Keyword ->
            ( Style3, "js-k" )

        DeclarationKeyword ->
            ( Style4, "js-dk" )

        FunctionEval ->
            ( Style4, "js-fe" )

        Function ->
            ( Style5, "js-f" )

        LiteralKeyword ->
            ( Style6, "js-lk" )

        Param ->
            ( Style7, "js-p" )

        ClassExtends ->
            ( Style5, "js-ce" )
