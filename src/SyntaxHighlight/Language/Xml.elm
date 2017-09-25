module SyntaxHighlight.Language.Xml
    exposing
        ( toLines
        , Syntax(..)
        , syntaxToStyle
          -- Exposing for tests purpose
        , toRevTokens
        )

import Char
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Language.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited, thenIgnore, consThen, addThen)
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = Tag
    | Attribute
    | AttributeValue


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
        [ whitespace |> map List.singleton
        , comment
        , keep oneOrMore (\c -> c /= '<' && not (isLineBreak c))
            |> map ((,) T.Normal >> List.singleton)
        , openTag
        ]


openTag : Parser (List Token)
openTag =
    (ignore oneOrMore ((==) '<')
        |. oneOf
            [ ignore (Exactly 1) (\c -> c == '/' || c == '!')
            , Parser.succeed ()
            ]
    )
        |> source
        |> map ((,) T.Normal >> List.singleton)
        |> andThen tag


tag : List Token -> Parser (List Token)
tag revTokens =
    oneOf
        [ ignore (Exactly 1) isStartTagChar
            |> thenIgnore zeroOrMore isTagChar
            |> source
            |> map ((,) (T.C Tag))
            |> andThen
                (\n ->
                    repeat zeroOrMore attributeLoop
                        |> map
                            ((::) (n :: revTokens)
                                >> List.reverse
                                >> List.concat
                            )
                )
        , succeed revTokens
        ]


isStartTagChar : Char -> Bool
isStartTagChar c =
    Char.isUpper c || Char.isLower c || Char.isDigit c


isTagChar : Char -> Bool
isTagChar c =
    isStartTagChar c || c == '-'


attributeLoop : Parser (List Token)
attributeLoop =
    oneOf
        [ keep oneOrMore isAttributeChar
            |> map ((,) (T.C Attribute))
            |> consThen attributeConfirm []
        , whitespace |> map List.singleton
        , keep oneOrMore (\c -> not (isWhitespace c) && c /= '>')
            |> map ((,) T.Normal >> List.singleton)
        ]


isAttributeChar : Char -> Bool
isAttributeChar c =
    isTagChar c || c == '_'


attributeConfirm : List Token -> Parser (List Token)
attributeConfirm revTokens =
    oneOf
        [ whitespace
            |> consThen attributeConfirm revTokens
        , keep (Exactly 1) ((==) '=')
            |> map ((,) T.Normal)
            |> consThen attributeValueLoop revTokens
        , succeed revTokens
        ]


attributeValueLoop : List Token -> Parser (List Token)
attributeValueLoop revTokens =
    oneOf
        [ whitespace
            |> consThen attributeValueLoop revTokens
        , attributeValue
            |> addThen succeed revTokens
        , succeed revTokens
        ]



-- Attribute Value


attributeValue : Parser (List Token)
attributeValue =
    oneOf
        [ doubleQuote
        , quote
        , ignore oneOrMore (\c -> not (isWhitespace c) && c /= '>')
            |> source
            |> map ((,) (T.C AttributeValue) >> List.singleton)
        ]


doubleQuote : Parser (List Token)
doubleQuote =
    delimited doubleQuoteDelimiter


doubleQuoteDelimiter : Delimiter Token
doubleQuoteDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = ((,) (T.C AttributeValue))
    , innerParsers = [ lineBreakList ]
    , isNotRelevant = not << isLineBreak
    }


quote : Parser (List Token)
quote =
    delimited
        { doubleQuoteDelimiter
            | start = "'"
            , end = "'"
        }



-- Comment


comment : Parser (List Token)
comment =
    delimited
        { doubleQuoteDelimiter
            | start = "<!--"
            , end = "-->"
            , defaultMap = ((,) T.Comment)
        }



-- Helpers


whitespace : Parser Token
whitespace =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) T.Normal)
        , lineBreak
        ]


lineBreak : Parser Token
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) T.LineBreak)


lineBreakList : Parser (List Token)
lineBreakList =
    repeat oneOrMore lineBreak


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        Tag ->
            ( Style3, "xml-t" )

        Attribute ->
            ( Style5, "xml-a" )

        AttributeValue ->
            ( Style2, "xlm-av" )
