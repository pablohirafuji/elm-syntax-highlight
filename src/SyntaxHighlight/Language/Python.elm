module SyntaxHighlight.Language.Python
    exposing
        ( toLines
          --, Syntax(..)
          --, syntaxToStyle
          -- Exposing for tests purpose
          --, toRevTokens
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
    if n == "def" then
        functionDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( T.C DeclarationKeyword, n ) ]
    else if n == "class" then
        classDeclarationLoop
            |> repeat zeroOrMore
            |> consThenRevConcat [ ( T.C DeclarationKeyword, n ) ]
    else if isKeyword n then
        succeed [ ( T.C Keyword, n ) ]
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
    -- TODO: handle base classes
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isIdentifierNameChar
            |> map ((,) (T.C Function) >> List.singleton)
        ]


isIdentifierNameChar : Char -> Bool
isIdentifierNameChar c =
    not
        (isPunctuation c
            || isStringLiteralChar c
            || isCommentChar c
            || isWhitespace c
        )


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')



-- Reserved words


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
        [ "finally"
        , "is"
        , "return"
        , "continue"
        , "for"
        , "lambda"
        , "try"
        , "from"
        , "nonlocal"
        , "while"
        , "and"
        , "del"
        , "global"
        , "not"
        , "with"
        , "as"
        , "elif"
        , "if"
        , "or"
        , "yield"
        , "assert"
        , "else"
        , "import"
        , "pass"
        , "break"
        , "except"
        , "in"
        , "raise"
        ]


isPunctuation : Char -> Bool
isPunctuation c =
    Set.member c punctuationSet


punctuationSet : Set Char
punctuationSet =
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
        [ "True"
        , "False"
        , "None"
        ]



-- String


stringLiteral : Parser (List Token)
stringLiteral =
    -- TODO: shortstring | longstring
    oneOf
        [ quote
        , doubleQuote
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

    -- TODO: escapable chars
    , innerParsers = [ lineBreak ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Token)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\''



-- Comments


comment : Parser (List Token)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Token)
inlineComment =
    symbol "#"
        |. ignore zeroOrMore (not << isLineBreak)
        |> source
        |> map ((,) T.Comment >> List.singleton)


multilineComment : Parser (List Token)
multilineComment =
    -- TODO: might not need this at all. just parse as multiline string?
    delimited
        { start = "'''"
        , end = "'''"
        , isNestable = False
        , defaultMap = ((,) T.Comment)
        , innerParsers = [ lineBreak ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '#'



-- Helpers


whitespaceOrComment : Parser (List Token)
whitespaceOrComment =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) T.Normal >> List.singleton)
        , lineBreak
        , comment
        ]


lineBreak : Parser (List Token)
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) T.LineBreak)
        |> repeat oneOrMore


number : Parser Token
number =
    SyntaxHighlight.Language.Helpers.number
        |> source
        |> map ((,) (T.C Number))


consThenRevConcat : List Token -> Parser (List (List Token)) -> Parser (List Token)
consThenRevConcat toCons =
    map ((::) toCons >> List.reverse >> List.concat)


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Number ->
            ( Style1, "py-n" )

        String ->
            ( Style2, "py-s" )

        Keyword ->
            ( Style3, "py-k" )

        DeclarationKeyword ->
            ( Style4, "py-dk" )

        Function ->
            ( Style5, "py-f" )

        LiteralKeyword ->
            ( Style6, "py-lk" )

        Param ->
            ( Style7, "py-p" )

        FunctionEval ->
            ( Default, "py-fe" )
