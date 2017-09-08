module SyntaxHighlight.Language.Xml
    exposing
        ( parse
          -- Exposing just for tests purpose
        , toSyntax
          -- Exposing just for tests purpose
        , SyntaxType(..)
        )

import Char
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed)
import SyntaxHighlight.Line exposing (Line, Fragment, Color(..))
import SyntaxHighlight.Line.Helpers exposing (toLines, normal)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited, thenIgnore, consThen, addThen)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | Tag
    | Attribute
    | AttributeValue
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
        [ whitespace |> map List.singleton
        , comment
        , keep oneOrMore (\c -> c /= '<' && not (isLineBreak c))
            |> map ((,) Normal >> List.singleton)
        , openTag
        ]


openTag : Parser (List Syntax)
openTag =
    (ignore oneOrMore ((==) '<')
        |. oneOf
            [ ignore (Exactly 1) (\c -> c == '/' || c == '!')
            , Parser.succeed ()
            ]
    )
        |> source
        |> map ((,) Normal >> List.singleton)
        |> andThen tag


tag : List Syntax -> Parser (List Syntax)
tag revSyntaxes =
    oneOf
        [ ignore (Exactly 1) isStartTagChar
            |> thenIgnore zeroOrMore isTagChar
            |> source
            |> map ((,) Tag)
            |> andThen
                (\n ->
                    repeat zeroOrMore attributeLoop
                        |> map
                            ((::) (n :: revSyntaxes)
                                >> List.reverse
                                >> List.concat
                            )
                )
        , succeed revSyntaxes
        ]


isStartTagChar : Char -> Bool
isStartTagChar c =
    Char.isUpper c || Char.isLower c || Char.isDigit c


isTagChar : Char -> Bool
isTagChar c =
    isStartTagChar c || c == '-'


attributeLoop : Parser (List Syntax)
attributeLoop =
    oneOf
        [ keep oneOrMore isAttributeChar
            |> map ((,) Attribute)
            |> consThen attributeConfirm []
        , whitespace |> map List.singleton
        , keep oneOrMore (\c -> not (isWhitespace c) && c /= '>')
            |> map ((,) Normal >> List.singleton)
        ]


isAttributeChar : Char -> Bool
isAttributeChar c =
    isTagChar c || c == '_'


attributeConfirm : List Syntax -> Parser (List Syntax)
attributeConfirm revSyntaxes =
    oneOf
        [ whitespace
            |> consThen attributeConfirm revSyntaxes
        , keep (Exactly 1) ((==) '=')
            |> map ((,) Normal)
            |> consThen attributeValueLoop revSyntaxes
        , succeed revSyntaxes
        ]


attributeValueLoop : List Syntax -> Parser (List Syntax)
attributeValueLoop revSyntaxes =
    oneOf
        [ whitespace
            |> consThen attributeValueLoop revSyntaxes
        , attributeValue
            |> addThen succeed revSyntaxes
        , succeed revSyntaxes
        ]



-- Attribute Value


attributeValue : Parser (List Syntax)
attributeValue =
    oneOf
        [ doubleQuote
        , quote
        , ignore oneOrMore (\c -> not (isWhitespace c) && c /= '>')
            |> source
            |> map ((,) AttributeValue >> List.singleton)
        ]


doubleQuote : Parser (List Syntax)
doubleQuote =
    delimited doubleQuoteDelimiter


doubleQuoteDelimiter : Delimiter Syntax
doubleQuoteDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = ((,) AttributeValue)
    , innerParsers = [ lineBreakList ]
    , isNotRelevant = not << isLineBreak
    }


quote : Parser (List Syntax)
quote =
    delimited
        { doubleQuoteDelimiter
            | start = "'"
            , end = "'"
        }



-- Comment


comment : Parser (List Syntax)
comment =
    delimited
        { doubleQuoteDelimiter
            | start = "<!--"
            , end = "-->"
            , defaultMap = ((,) Comment)
        }



-- Helpers


whitespace : Parser Syntax
whitespace =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) Normal)
        , lineBreak
        ]


lineBreak : Parser Syntax
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) LineBreak)


lineBreakList : Parser (List Syntax)
lineBreakList =
    repeat oneOrMore lineBreak


toFragment : Syntax -> Fragment
toFragment ( syntaxType, text ) =
    case syntaxType of
        Normal ->
            normal Default text

        Comment ->
            normal Color1 text

        Tag ->
            normal Color3 text

        Attribute ->
            normal Color5 text

        AttributeValue ->
            normal Color2 text

        LineBreak ->
            normal Default text
