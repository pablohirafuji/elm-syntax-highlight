module SyntaxHighlight.Language.Elm
    exposing
        ( toLines
        , Syntax(..)
        , syntaxToStyle
          -- Exposing for tests purpose
        , toRevTokens
        )

import Char
import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, delayedCommit, repeat, succeed)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Language.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, number, delimited, thenIgnore, isEscapable, escapable, consThen, addThen)
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = String
    | BasicSymbol
    | GroupSymbol
    | Capitalized
    | Keyword
    | Function
    | TypeSignature
    | Number


toLines : String -> Result Error (List Line)
toLines =
    toRevTokens
        >> Result.map (Line.toLines syntaxToStyle)


toRevTokens : String -> Result Error (List Token)
toRevTokens =
    lineStart []
        |> repeat zeroOrMore
        |> map (List.reverse >> List.concat)
        |> Parser.run


lineStart : List Token -> Parser (List Token)
lineStart revTokens =
    oneOf
        [ whitespaceOrComment succeed revTokens
        , variable |> andThen (lineStartVariable revTokens)
        , stringLiteral |> addThen functionBody revTokens
        , functionBodyContent |> consThen functionBody revTokens
        ]


lineStartVariable : List Token -> String -> Parser (List Token)
lineStartVariable revTokens n =
    if n == "module" || n == "import" then
        moduleDeclaration (( T.C Keyword, n ) :: revTokens)
    else if n == "port" then
        portDeclaration (( T.C Keyword, n ) :: revTokens)
    else if isKeyword n then
        functionBody (( T.C Keyword, n ) :: revTokens)
    else
        functionSignature (( T.C Function, n ) :: revTokens)



-- Module Declaration


moduleDeclaration : List Token -> Parser (List Token)
moduleDeclaration revTokens =
    oneOf
        [ whitespaceOrComment moduleDeclaration revTokens
        , symbol "("
            |> map (always ( T.Normal, "(" ))
            |> consThen modDecParentheses revTokens
        , oneOf
            [ commentChar |> map ((,) T.Normal)
            , keyword "exposing"
                |> map (always ( T.C Keyword, "exposing" ))
            , keyword "as"
                |> map (always ( T.C Keyword, "as" ))
            , keep oneOrMore modDecIsNotRelevant
                |> map ((,) T.Normal)
            ]
            |> consThen moduleDeclaration revTokens
        , succeed revTokens
        ]


modDecIsNotRelevant : Char -> Bool
modDecIsNotRelevant c =
    not (isWhitespace c || isCommentChar c || c == '(')


modDecParentheses : List Token -> Parser (List Token)
modDecParentheses revTokens =
    oneOf
        [ whitespaceOrComment modDecParentheses revTokens
        , symbol ")"
            |> map (always ( T.Normal, ")" ))
            |> consThen moduleDeclaration revTokens
        , oneOf
            [ infixParser
            , commentChar |> map ((,) T.Normal)
            , keep oneOrMore (\c -> c == ',' || c == '.')
                |> map ((,) T.Normal)
            , ignore (Exactly 1) Char.isUpper
                |> thenIgnore zeroOrMore mdpIsNotRelevant
                |> source
                |> map ((,) (T.C TypeSignature))
            , keep oneOrMore mdpIsNotRelevant
                |> map ((,) (T.C Function))
            ]
            |> consThen modDecParentheses revTokens
        , symbol "("
            |> map (always ( T.Normal, "(" ))
            |> consThen (modDecParNest 0) revTokens
        , succeed revTokens
        ]


mdpIsNotRelevant : Char -> Bool
mdpIsNotRelevant c =
    not (isWhitespace c || isCommentChar c || c == '(' || c == ')' || c == ',' || c == '.')


modDecParNest : Int -> List Token -> Parser (List Token)
modDecParNest nestLevel revTokens =
    oneOf
        [ whitespaceOrComment (modDecParNest nestLevel) revTokens
        , symbol "("
            |> map (always ( T.Normal, "(" ))
            |> andThen (\n -> modDecParNest (nestLevel + 1) (n :: revTokens))
        , symbol ")"
            |> map (always ( T.Normal, ")" ))
            |> andThen
                (\n ->
                    if nestLevel == 0 then
                        modDecParentheses (n :: revTokens)
                    else
                        modDecParNest (nestLevel - 1) (n :: revTokens)
                )
        , oneOf
            [ commentChar |> map ((,) T.Normal)
            , keep oneOrMore (not << mdpnIsSpecialChar)
                |> map ((,) T.Normal)
            ]
            |> consThen (modDecParNest nestLevel) revTokens
        , succeed revTokens
        ]


mdpnIsSpecialChar : Char -> Bool
mdpnIsSpecialChar c =
    isLineBreak c || isCommentChar c || c == '(' || c == ')'



-- Port Declaration


portDeclaration : List Token -> Parser (List Token)
portDeclaration revTokens =
    oneOf
        [ whitespaceOrComment portDeclaration revTokens
        , variable |> andThen (portDeclarationHelp revTokens)
        , functionBody revTokens
        ]


portDeclarationHelp : List Token -> String -> Parser (List Token)
portDeclarationHelp revTokens str =
    if str == "module" then
        moduleDeclaration (( T.C Keyword, str ) :: revTokens)
    else
        functionSignature (( T.C Function, str ) :: revTokens)



-- Function Signature


functionSignature : List Token -> Parser (List Token)
functionSignature revTokens =
    oneOf
        [ symbol ":"
            |> map (always ( T.C BasicSymbol, ":" ))
            |> consThen fnSigContent revTokens
        , whitespaceOrComment functionSignature revTokens
        , functionBody revTokens
        ]


fnSigContent : List Token -> Parser (List Token)
fnSigContent revTokens =
    oneOf
        [ whitespaceOrComment fnSigContent revTokens
        , fnSigContentHelp |> consThen fnSigContent revTokens
        , succeed revTokens
        ]


fnSigContentHelp : Parser Token
fnSigContentHelp =
    oneOf
        [ symbol "()" |> map (always ( T.C TypeSignature, "()" ))
        , symbol "->" |> map (always ( T.C BasicSymbol, "->" ))
        , keep oneOrMore (\c -> c == '(' || c == ')' || c == '-' || c == ',')
            |> map ((,) T.Normal)
        , ignore (Exactly 1) Char.isUpper
            |> thenIgnore zeroOrMore fnSigIsNotRelevant
            |> source
            |> map ((,) (T.C TypeSignature))
        , keep oneOrMore fnSigIsNotRelevant |> map ((,) T.Normal)
        ]


fnSigIsNotRelevant : Char -> Bool
fnSigIsNotRelevant c =
    not (isWhitespace c || c == '(' || c == ')' || c == '-' || c == ',')



-- Function Body


functionBody : List Token -> Parser (List Token)
functionBody revTokens =
    oneOf
        [ whitespaceOrComment functionBody revTokens
        , stringLiteral |> addThen functionBody revTokens
        , functionBodyContent |> consThen functionBody revTokens
        , succeed revTokens
        ]


functionBodyContent : Parser Token
functionBodyContent =
    oneOf
        [ number |> source |> map ((,) (T.C Number))
        , symbol "()" |> map (always ( T.C Capitalized, "()" ))
        , infixParser
        , basicSymbol |> map ((,) (T.C BasicSymbol))
        , groupSymbol |> map ((,) (T.C GroupSymbol))
        , capitalized |> map ((,) (T.C Capitalized))
        , variable
            |> map
                (\n ->
                    if isKeyword n then
                        ( T.C Keyword, n )
                    else
                        ( T.Normal, n )
                )
        , weirdText |> map ((,) T.Normal)
        ]


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
        [ "as"
        , "where"
        , "let"
        , "in"
        , "if"
        , "else"
        , "then"
        , "case"
        , "of"
        , "type"
        , "alias"
        ]


basicSymbol : Parser String
basicSymbol =
    keep oneOrMore isBasicSymbol


isBasicSymbol : Char -> Bool
isBasicSymbol c =
    Set.member c basicSymbols


basicSymbols : Set Char
basicSymbols =
    Set.fromList
        [ '|'
        , '.'
        , '='
        , '\\'
        , '/'
        , '('
        , ')'
        , '-'
        , '>'
        , '<'
        , ':'
        , '+'
        , '!'
        , '$'
        , '%'
        , '&'
        , '*'
        ]


groupSymbol : Parser String
groupSymbol =
    keep oneOrMore isGroupSymbol


isGroupSymbol : Char -> Bool
isGroupSymbol c =
    Set.member c groupSymbols


groupSymbols : Set Char
groupSymbols =
    Set.fromList
        [ ','
        , '['
        , ']'
        , '{'
        , '}'
        ]


capitalized : Parser String
capitalized =
    ignore (Exactly 1) Char.isUpper
        |> thenIgnore zeroOrMore isVariableChar
        |> source


variable : Parser String
variable =
    ignore (Exactly 1) Char.isLower
        |> thenIgnore zeroOrMore isVariableChar
        |> source


isVariableChar : Char -> Bool
isVariableChar c =
    not
        (isWhitespace c
            || isBasicSymbol c
            || isGroupSymbol c
            || isStringLiteralChar c
        )


weirdText : Parser String
weirdText =
    keep oneOrMore isVariableChar



-- Infix


infixParser : Parser Token
infixParser =
    delayedCommit (symbol "(")
        (delayedCommit (ignore oneOrMore isInfixChar) (symbol ")"))
        |> source
        |> map ((,) (T.C Function))


isInfixChar : Char -> Bool
isInfixChar c =
    Set.member c infixSet


infixSet : Set Char
infixSet =
    Set.fromList
        [ '+'
        , '-'
        , '/'
        , '*'
        , '='
        , '.'
        , '$'
        , '<'
        , '>'
        , ':'
        , '&'
        , '|'
        , '^'
        , '?'
        , '%'
        , '#'
        , '@'
        , '~'
        , '!'
        , ','
        ]



-- String/Char literals


stringLiteral : Parser (List Token)
stringLiteral =
    oneOf
        [ tripleDoubleQuote
        , doubleQuote
        , quote
        ]


doubleQuote : Parser (List Token)
doubleQuote =
    delimited stringDelimiter


stringDelimiter : Delimiter Token
stringDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = ((,) (T.C String))
    , innerParsers = [ lineBreakList, elmEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


tripleDoubleQuote : Parser (List Token)
tripleDoubleQuote =
    delimited
        { stringDelimiter
            | start = "\"\"\""
            , end = "\"\"\""
        }


quote : Parser (List Token)
quote =
    delimited
        { stringDelimiter
            | start = "'"
            , end = "'"
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
    symbol "--"
        |> thenIgnore zeroOrMore (not << isLineBreak)
        |> source
        |> map ((,) T.Comment >> List.singleton)


multilineComment : Parser (List Token)
multilineComment =
    delimited
        { start = "{-"
        , end = "-}"
        , isNestable = True
        , defaultMap = ((,) T.Comment)
        , innerParsers = [ lineBreakList ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


commentChar : Parser String
commentChar =
    keep (Exactly 1) isCommentChar


isCommentChar : Char -> Bool
isCommentChar c =
    c == '-' || c == '{'



-- Helpers


whitespaceOrComment : (List Token -> Parser (List Token)) -> List Token -> Parser (List Token)
whitespaceOrComment continueFunction revTokens =
    oneOf
        [ space |> consThen continueFunction revTokens
        , lineBreak
            |> consThen (checkContext continueFunction) revTokens
        , comment |> addThen continueFunction revTokens
        ]


checkContext : (List Token -> Parser (List Token)) -> List Token -> Parser (List Token)
checkContext continueFunction revTokens =
    oneOf
        [ whitespaceOrComment continueFunction revTokens
        , succeed revTokens
        ]


space : Parser Token
space =
    keep oneOrMore isSpace
        |> map ((,) T.Normal)


lineBreak : Parser Token
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) T.LineBreak)


lineBreakList : Parser (List Token)
lineBreakList =
    repeat oneOrMore lineBreak


elmEscapable : Parser (List Token)
elmEscapable =
    escapable
        |> source
        |> map ((,) (T.C Capitalized))
        |> repeat oneOrMore


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        String ->
            ( Style2, "elm-s" )

        BasicSymbol ->
            ( Style3, "elm-bs" )

        GroupSymbol ->
            ( Style4, "elm-gs" )

        Capitalized ->
            ( Style6, "elm-c" )

        Keyword ->
            ( Style3, "elm-k" )

        Function ->
            ( Style5, "elm-f" )

        TypeSignature ->
            ( Style4, "elm-ts" )

        Number ->
            ( Style1, "elm-n" )
