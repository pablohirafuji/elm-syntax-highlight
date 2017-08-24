module SyntaxHighlight.Language.Elm
    exposing
        ( parse
          -- Exposing just for tests purpose
        , toSyntax
          -- Exposing just for tests purpose
        , SyntaxType(..)
        )

import Char
import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, delayedCommit, repeat, succeed)
import SyntaxHighlight.Line exposing (Line, Fragment, Color(..))
import SyntaxHighlight.Line.Helpers exposing (toLines, normal, emphasis, strong)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, number, delimited, thenIgnore, isEscapable, escapable, consThen, addThen)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | String
    | BasicSymbol
    | GroupSymbol
    | Capitalized
    | Keyword
    | Function
    | TypeSignature
    | Space
    | LineBreak
    | Number


parse : String -> Result Error (List Line)
parse =
    toSyntax
        >> Result.map (toLines LineBreak toFragment)


toSyntax : String -> Result Error (List Syntax)
toSyntax =
    lineStart []
        |> repeat zeroOrMore
        |> map (List.reverse >> List.concat)
        |> Parser.run


lineStart : List Syntax -> Parser (List Syntax)
lineStart revSyntaxes =
    oneOf
        [ whitespaceOrComment succeed revSyntaxes
        , variable |> andThen (lineStartVariable revSyntaxes)
        , stringLiteral |> addThen functionBody revSyntaxes
        , functionBodyContent |> consThen functionBody revSyntaxes
        ]


lineStartVariable : List Syntax -> String -> Parser (List Syntax)
lineStartVariable revSyntaxes n =
    if n == "module" || n == "import" then
        moduleDeclaration (( Keyword, n ) :: revSyntaxes)
    else if n == "port" then
        portDeclaration (( Keyword, n ) :: revSyntaxes)
    else if isKeyword n then
        functionBody (( Keyword, n ) :: revSyntaxes)
    else
        functionSignature (( Function, n ) :: revSyntaxes)



-- Module Declaration


moduleDeclaration : List Syntax -> Parser (List Syntax)
moduleDeclaration revSyntaxes =
    oneOf
        [ whitespaceOrComment moduleDeclaration revSyntaxes
        , symbol "("
            |> map (always ( Normal, "(" ))
            |> consThen modDecParentheses revSyntaxes
        , oneOf
            [ commentChar |> map ((,) Normal)
            , keyword "exposing" |> map (always ( Keyword, "exposing" ))
            , keyword "as" |> map (always ( Keyword, "as" ))
            , keep oneOrMore modDecIsNotRelevant |> map ((,) Normal)
            ]
            |> consThen moduleDeclaration revSyntaxes
        , succeed revSyntaxes
        ]


modDecIsNotRelevant : Char -> Bool
modDecIsNotRelevant c =
    not (isWhitespace c || isCommentChar c || c == '(')


modDecParentheses : List Syntax -> Parser (List Syntax)
modDecParentheses revSyntaxes =
    oneOf
        [ whitespaceOrComment modDecParentheses revSyntaxes
        , symbol ")"
            |> map (always ( Normal, ")" ))
            |> consThen moduleDeclaration revSyntaxes
        , oneOf
            [ infixParser
            , commentChar |> map ((,) Normal)
            , keep oneOrMore (\c -> c == ',' || c == '.') |> map ((,) Normal)
            , ignore (Exactly 1) Char.isUpper
                |> thenIgnore zeroOrMore mdpIsNotRelevant
                |> source
                |> map ((,) TypeSignature)
            , keep oneOrMore mdpIsNotRelevant |> map ((,) Function)
            ]
            |> consThen modDecParentheses revSyntaxes
        , symbol "("
            |> map (always ( Normal, "(" ))
            |> consThen (modDecParNest 0) revSyntaxes
        , succeed revSyntaxes
        ]


mdpIsNotRelevant : Char -> Bool
mdpIsNotRelevant c =
    not (isWhitespace c || isCommentChar c || c == '(' || c == ')' || c == ',' || c == '.')


modDecParNest : Int -> List Syntax -> Parser (List Syntax)
modDecParNest nestLevel revSyntaxes =
    oneOf
        [ whitespaceOrComment (modDecParNest nestLevel) revSyntaxes
        , symbol "("
            |> map (always ( Normal, "(" ))
            |> andThen (\n -> modDecParNest (nestLevel + 1) (n :: revSyntaxes))
        , symbol ")"
            |> map (always ( Normal, ")" ))
            |> andThen
                (\n ->
                    if nestLevel == 0 then
                        modDecParentheses (n :: revSyntaxes)
                    else
                        modDecParNest (nestLevel - 1) (n :: revSyntaxes)
                )
        , oneOf
            [ commentChar |> map ((,) Normal)
            , keep oneOrMore (not << mdpnIsSpecialChar) |> map ((,) Normal)
            ]
            |> consThen (modDecParNest nestLevel) revSyntaxes
        , succeed revSyntaxes
        ]


mdpnIsSpecialChar : Char -> Bool
mdpnIsSpecialChar c =
    isLineBreak c || isCommentChar c || c == '(' || c == ')'



-- Port Declaration


portDeclaration : List Syntax -> Parser (List Syntax)
portDeclaration revSyntaxes =
    oneOf
        [ whitespaceOrComment portDeclaration revSyntaxes
        , variable |> andThen (portDeclarationHelp revSyntaxes)
        , functionBody revSyntaxes
        ]


portDeclarationHelp : List Syntax -> String -> Parser (List Syntax)
portDeclarationHelp revSyntaxes str =
    if str == "module" then
        moduleDeclaration (( Keyword, str ) :: revSyntaxes)
    else
        functionSignature (( Function, str ) :: revSyntaxes)



-- Function Signature


functionSignature : List Syntax -> Parser (List Syntax)
functionSignature revSyntaxes =
    oneOf
        [ symbol ":"
            |> map (always ( BasicSymbol, ":" ))
            |> consThen fnSigContent revSyntaxes
        , whitespaceOrComment functionSignature revSyntaxes
        , functionBody revSyntaxes
        ]


fnSigContent : List Syntax -> Parser (List Syntax)
fnSigContent revSyntaxes =
    oneOf
        [ whitespaceOrComment fnSigContent revSyntaxes
        , fnSigContentHelp |> consThen fnSigContent revSyntaxes
        , succeed revSyntaxes
        ]


fnSigContentHelp : Parser Syntax
fnSigContentHelp =
    oneOf
        [ symbol "()" |> map (always ( TypeSignature, "()" ))
        , symbol "->" |> map (always ( BasicSymbol, "->" ))
        , keep oneOrMore (\c -> c == '(' || c == ')' || c == '-' || c == ',')
            |> map ((,) Normal)
        , ignore (Exactly 1) Char.isUpper
            |> thenIgnore zeroOrMore fnSigIsNotRelevant
            |> source
            |> map ((,) TypeSignature)
        , keep oneOrMore fnSigIsNotRelevant |> map ((,) Normal)
        ]


fnSigIsNotRelevant : Char -> Bool
fnSigIsNotRelevant c =
    not (isWhitespace c || c == '(' || c == ')' || c == '-' || c == ',')



-- Function Body


functionBody : List Syntax -> Parser (List Syntax)
functionBody revSyntaxes =
    oneOf
        [ whitespaceOrComment functionBody revSyntaxes
        , stringLiteral |> addThen functionBody revSyntaxes
        , functionBodyContent |> consThen functionBody revSyntaxes
        , succeed revSyntaxes
        ]


functionBodyContent : Parser Syntax
functionBodyContent =
    oneOf
        [ number |> source |> map ((,) Number)
        , symbol "()" |> map (always ( Capitalized, "()" ))
        , infixParser
        , basicSymbol |> map ((,) BasicSymbol)
        , groupSymbol |> map ((,) GroupSymbol)
        , capitalized |> map ((,) Capitalized)
        , variable
            |> map
                (\n ->
                    if isKeyword n then
                        ( Keyword, n )
                    else
                        ( Normal, n )
                )
        , weirdText |> map ((,) Normal)
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


infixParser : Parser Syntax
infixParser =
    delayedCommit (symbol "(")
        (delayedCommit (ignore oneOrMore isInfixChar) (symbol ")"))
        |> source
        |> map ((,) Function)


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


stringLiteral : Parser (List Syntax)
stringLiteral =
    oneOf
        [ tripleDoubleQuote
        , doubleQuote
        , quote
        ]


doubleQuote : Parser (List Syntax)
doubleQuote =
    delimited stringDelimiter


stringDelimiter : Delimiter Syntax
stringDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = ((,) String)
    , innerParsers = [ lineBreakList, elmEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


tripleDoubleQuote : Parser (List Syntax)
tripleDoubleQuote =
    delimited
        { stringDelimiter
            | start = "\"\"\""
            , end = "\"\"\""
        }


quote : Parser (List Syntax)
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


comment : Parser (List Syntax)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Syntax)
inlineComment =
    symbol "--"
        |> thenIgnore zeroOrMore (not << isLineBreak)
        |> source
        |> map ((,) Comment >> List.singleton)


multilineComment : Parser (List Syntax)
multilineComment =
    delimited
        { start = "{-"
        , end = "-}"
        , isNestable = True
        , defaultMap = ((,) Comment)
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


whitespaceOrComment : (List Syntax -> Parser (List Syntax)) -> List Syntax -> Parser (List Syntax)
whitespaceOrComment continueFunction revSyntaxes =
    oneOf
        [ space |> consThen continueFunction revSyntaxes
        , lineBreak
            |> consThen (checkContext continueFunction) revSyntaxes
        , comment |> addThen continueFunction revSyntaxes
        ]


checkContext : (List Syntax -> Parser (List Syntax)) -> List Syntax -> Parser (List Syntax)
checkContext continueFunction revSyntaxes =
    oneOf
        [ whitespaceOrComment continueFunction revSyntaxes
        , succeed revSyntaxes
        ]


space : Parser Syntax
space =
    keep oneOrMore isSpace
        |> map ((,) Space)


lineBreak : Parser Syntax
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) LineBreak)


lineBreakList : Parser (List Syntax)
lineBreakList =
    repeat oneOrMore lineBreak


elmEscapable : Parser (List Syntax)
elmEscapable =
    escapable
        |> source
        |> map ((,) Capitalized)
        |> repeat oneOrMore


toFragment : Syntax -> Fragment
toFragment ( syntaxType, text ) =
    case syntaxType of
        Normal ->
            normal Default text

        Comment ->
            normal Color1 text

        String ->
            normal Color2 text

        BasicSymbol ->
            normal Color3 text

        GroupSymbol ->
            normal Color4 text

        Capitalized ->
            normal Color6 text

        Keyword ->
            normal Color3 text

        Function ->
            normal Color5 text

        TypeSignature ->
            emphasis Color4 text

        Space ->
            normal Default text

        LineBreak ->
            normal Default text

        Number ->
            normal Color6 text
