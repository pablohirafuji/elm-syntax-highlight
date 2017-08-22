module SyntaxHighlight.Language.Css exposing (parse)

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat)
import SyntaxHighlight.Line exposing (Line, Fragment, Color(..))
import SyntaxHighlight.Line.Helpers exposing (toLines, normal, emphasis, strong)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited, escapable, isEscapable, addThen, consThen)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | String
    | Element
    | Id
    | Class
    | Attribute
    | AttributeValue
    | Number
    | Grandeza
    | LineBreak


parse : String -> Result Error (List Line)
parse =
    Parser.run (mainLoop [])
        >> Result.map (toLines LineBreak toFragment)


mainLoop : List Syntax -> Parser (List Syntax)
mainLoop revSyntaxes =
    oneOf
        [ whitespaceOrComment mainLoop revSyntaxes
        , stringLiteral
            |> addThen mainLoop revSyntaxes
        , oneOf
            [ operatorChar
            , groupChar
            , number
            ]
            |> consThen mainLoop revSyntaxes
        , keep oneOrMore isIdentifierNameChar
            |> andThen (keywordParser revSyntaxes)
        , end revSyntaxes
        ]


keywordParser : List Syntax -> String -> Parser (List Syntax)
keywordParser revSyntaxes n =
    if n == "function" || n == "static" then
        functionStatementLoop (( DeclarationKeyword, n ) :: revSyntaxes)
    else if n == "class" then
        classStatementLoop (( DeclarationKeyword, n ) :: revSyntaxes)
    else if n == "this" || n == "super" then
        mainLoop (( Param, n ) :: revSyntaxes)
    else if n == "constructor" then
        functionStatementLoop (( Function, n ) :: revSyntaxes)
    else if isKeyword n then
        mainLoop (( Keyword, n ) :: revSyntaxes)
    else if isDeclarationKeyword n then
        mainLoop (( DeclarationKeyword, n ) :: revSyntaxes)
    else if isLiteralKeyword n then
        mainLoop (( LiteralKeyword, n ) :: revSyntaxes)
    else
        functionEvalLoop n revSyntaxes []


functionStatementLoop : List Syntax -> Parser (List Syntax)
functionStatementLoop revSyntaxes =
    oneOf
        [ whitespaceOrComment functionStatementLoop revSyntaxes
        , keep oneOrMore isIdentifierNameChar
            |> andThen (\n -> functionStatementLoop (( Function, n ) :: revSyntaxes))
        , symbol "("
            |> andThen (\n -> argLoop (( Normal, "(" ) :: revSyntaxes))
        , mainLoop revSyntaxes
        ]


functionEvalLoop : String -> List Syntax -> List Syntax -> Parser (List Syntax)
functionEvalLoop identifier preSyntaxList postSyntaxList =
    oneOf
        [ whitespaceOrComment (functionEvalLoop identifier preSyntaxList) postSyntaxList
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
classStatementLoop revSyntaxes =
    oneOf
        [ whitespaceOrComment classStatementLoop revSyntaxes
        , keep oneOrMore isIdentifierNameChar
            |> map ((,) Function)
            |> consThen mainLoop revSyntaxes
        , mainLoop revSyntaxes
        ]


argLoop : List Syntax -> Parser (List Syntax)
argLoop revSyntaxes =
    oneOf
        [ whitespaceOrComment argLoop revSyntaxes
        , keep oneOrMore (\c -> not (isCommentChar c || isWhitespace c || c == ',' || c == ')'))
            |> andThen (\n -> argLoop (( Param, n ) :: revSyntaxes))
        , keep oneOrMore (\c -> c == '/' || c == ',')
            |> andThen (\n -> argLoop (( Normal, n ) :: revSyntaxes))
        , symbol ")"
            |> andThen (\n -> mainLoop (( Normal, ")" ) :: revSyntaxes))
        , end revSyntaxes
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
        ]


symbolChar : Parser Syntax
symbolChar =
    keep oneOrMore isSymbolChar
        |> map ((,) Normal)


isSymbolChar : Char -> Bool
isSymbolChar c =
    Set.member c symbolSet


symbolSet : Set Char
symbolSet =
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
        , '\\'
        , '?'
        , '^'
        , ':'
        , ';'
        , '~'
        , '%'
        , '.'
        , '{'
        , '}'
        , '('
        , ')'
        , '['
        , ']'
        , ','
        , '@'
        , '#'
        , '$'
        , '"'
        , '\''
        ]


isLiteralKeyword : String -> Bool
isLiteralKeyword str =
    Set.member str literalKeywordSet


literalKeywordSet : Set String
literalKeywordSet =
    [ "true"
    , "false"
    , "null"
    , "undefined"
    , "NaN"
    , "Infinity"
    ]
        |> Set.fromList



-- String literal


stringLiteral : Parser (List Syntax)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
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
    , innerParsers = [ lineBreakList, cssEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Syntax)
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


comment : Parser (List Syntax)
comment =
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


whitespaceOrComment : (List Syntax -> Parser (List Syntax)) -> List Syntax -> Parser (List Syntax)
whitespaceOrComment continueFunction revSyntaxes =
    oneOf
        [ oneOf
            [ keep oneOrMore isSpace |> map ((,) Normal)
            , lineBreak
            ]
            |> consThen continueFunction revSyntaxes
        , comment |> addThen continueFunction revSyntaxes
        ]


lineBreak : Parser Syntax
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) LineBreak)


lineBreakList : Parser (List Syntax)
lineBreakList =
    repeat oneOrMore lineBreak


number : Parser Syntax
number =
    SyntaxHighlight.Helpers.number
        |> source
        |> map ((,) Number)


cssEscapable : Parser (List Syntax)
cssEscapable =
    escapable
        |> source
        |> map ((,) Number)
        |> repeat oneOrMore


end : List Syntax -> Parser (List Syntax)
end revSyntaxes =
    Parser.end
        |> map (always revSyntaxes)


toFragment : Syntax -> Fragment
toFragment ( syntaxType, text ) =
    case syntaxType of
        Normal ->
            normal Default text

        Comment ->
            normal Color1 text

        String ->
            normal Color2 text

        Element ->
            normal Color3 text

        Id ->
            normal Color5 text

        Class ->
            normal Color5 text

        Attribute ->
            emphasis Color7 text

        AttributeValue ->
            normal Color4 text

        Number ->
            normal Color6 text

        Grandeza ->
            normal Color3 text

        LineBreak ->
            normal Default text
