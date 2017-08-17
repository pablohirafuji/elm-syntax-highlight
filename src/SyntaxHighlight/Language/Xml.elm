module SyntaxHighlight.Language.Xml
    exposing
        ( parse
          -- Exposing just for tests purpose
        , toSyntax
          -- Exposing just for tests purpose
        , SyntaxType(..)
        )

import Char
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen)
import SyntaxHighlight.Line exposing (Line, newLine, Fragment, Color(..), normal)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited)


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
        >> Result.map toLines


toSyntax : String -> Result Error (List Syntax)
toSyntax =
    Parser.run (mainLoop [])


mainLoop : List Syntax -> Parser (List Syntax)
mainLoop revSyntaxList =
    oneOf
        [ whitespace
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , comment
            |> andThen (\n -> mainLoop (n ++ revSyntaxList))
        , keep oneOrMore (\c -> c /= '<' && not (isLineBreak c))
            |> map ((,) Normal)
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , openTag revSyntaxList
        , end revSyntaxList
        ]


openTag : List Syntax -> Parser (List Syntax)
openTag revSyntaxList =
    (ignore oneOrMore ((==) '<')
        |. oneOf
            [ ignore (Exactly 1) (\c -> c == '/' || c == '!')
            , Parser.succeed ()
            ]
    )
        |> source
        |> map ((,) Normal)
        |> andThen (\n -> tag (n :: revSyntaxList))


endTag : List Syntax -> Parser (List Syntax)
endTag revSyntaxList =
    keep oneOrMore ((==) '>')
        |> map ((,) Normal)
        |> andThen (\n -> mainLoop (n :: revSyntaxList))


tag : List Syntax -> Parser (List Syntax)
tag revSyntaxList =
    oneOf
        [ (ignore (Exactly 1) isStartTagChar
            |. ignore zeroOrMore isTagChar
          )
            |> source
            |> map ((,) Tag)
            |> andThen (\n -> attributeLoop (n :: revSyntaxList))
        , chompUntilEndTag revSyntaxList
        ]


chompUntilEndTag : List Syntax -> Parser (List Syntax)
chompUntilEndTag revSyntaxList =
    (ignore zeroOrMore (\c -> c /= '>' && not (isLineBreak c))
        |. ignore zeroOrMore ((==) '>')
    )
        |> source
        |> map ((,) Normal)
        |> andThen (\n -> mainLoop (n :: revSyntaxList))


isStartTagChar : Char -> Bool
isStartTagChar c =
    Char.isUpper c || Char.isLower c || Char.isDigit c


isTagChar : Char -> Bool
isTagChar c =
    isStartTagChar c || c == '-'


attributeLoop : List Syntax -> Parser (List Syntax)
attributeLoop revSyntaxList =
    oneOf
        [ keep oneOrMore isAttributeChar
            |> map ((,) Attribute)
            |> andThen (\n -> attributeConfirm (n :: revSyntaxList))
        , whitespace
            |> andThen (\n -> attributeLoop (n :: revSyntaxList))
        , endTag revSyntaxList
        , keep oneOrMore (\c -> not (isWhitespace c) && c /= '>')
            |> map ((,) Normal)
            |> andThen (\n -> attributeLoop (n :: revSyntaxList))
        , end revSyntaxList
        ]


isAttributeChar : Char -> Bool
isAttributeChar c =
    isTagChar c || c == '_'


attributeConfirm : List Syntax -> Parser (List Syntax)
attributeConfirm revSyntaxList =
    oneOf
        [ whitespace
            |> andThen (\n -> attributeConfirm (n :: revSyntaxList))
        , keep (Exactly 1) ((==) '=')
            |> map ((,) Normal)
            |> andThen (\n -> attributeValueLoop (n :: revSyntaxList))
        , endTag revSyntaxList
        , attributeLoop revSyntaxList
        ]


attributeValueLoop : List Syntax -> Parser (List Syntax)
attributeValueLoop revSyntaxList =
    oneOf
        [ whitespace
            |> andThen (\n -> attributeValueLoop (n :: revSyntaxList))
        , attributeValue
            |> andThen (\n -> attributeLoop (n ++ revSyntaxList))
        , endTag revSyntaxList
        , end revSyntaxList
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
    , innerParsers = [ lineBreak ]
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


end : List Syntax -> Parser (List Syntax)
end revSyntaxList =
    Parser.end
        |> map (always revSyntaxList)


toLines : List Syntax -> List Line
toLines revSyntaxList =
    List.foldl toLinesHelp ( [], [] ) revSyntaxList
        |> (\( lines, frags ) -> newLine frags :: lines)


toLinesHelp : Syntax -> ( List Line, List Fragment ) -> ( List Line, List Fragment )
toLinesHelp ( syntaxType, text ) ( lines, fragments ) =
    if syntaxType == LineBreak then
        ( newLine fragments :: lines
        , [ normal Default text ]
        )
    else
        ( lines, toFragment ( syntaxType, text ) :: fragments )


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
