module SyntaxHighlight.Language.Xml exposing (..)

import Char
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen)
import SyntaxHighlight.Fragment exposing (Fragment, Color(..), normal)
import SyntaxHighlight.Helpers exposing (isWhitespace, isSpace, isLineBreak, delimited)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | Tag
    | Attribute
    | AttributeValue


parse : String -> Result Error (List Fragment)
parse =
    Parser.run (mainLoop [])
        >> Result.map (List.map syntaxToFragment)


mainLoop : List Syntax -> Parser (List Syntax)
mainLoop revSyntaxList =
    oneOf
        [ keep oneOrMore (\c -> c /= '<')
            |> map ((,) Normal)
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , comment
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , startTag revSyntaxList
        , end revSyntaxList
        ]


startTag : List Syntax -> Parser (List Syntax)
startTag revSyntaxList =
    (ignore oneOrMore ((==) '<')
        |. oneOf
            [ ignore (Exactly 1) ((==) '/')
            , ignore (Exactly 1) ((==) '!')
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


chompUntilEndTag : List Syntax -> Parser (List Syntax)
chompUntilEndTag revSyntaxList =
    (ignore zeroOrMore ((/=) '>')
        |. ignore zeroOrMore ((==) '>')
    )
        |> source
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
            |> andThen (\n -> attributeLoop (n :: revSyntaxList))
        , endTag revSyntaxList
        , end revSyntaxList
        ]



-- String


attributeValue : Parser Syntax
attributeValue =
    oneOf
        [ doubleQuote
        , singleQuote
        , ignore oneOrMore (\c -> not (isWhitespace c) && c /= '>')
        ]
        |> source
        |> map ((,) AttributeValue)


doubleQuote : Parser ()
doubleQuote =
    delimited
        { start = "\""
        , end = "\""
        , isNestable = False
        , isEscapable = False
        }


singleQuote : Parser ()
singleQuote =
    delimited
        { start = "'"
        , end = "'"
        , isNestable = False
        , isEscapable = False
        }



-- Comment


comment : Parser Syntax
comment =
    delimited
        { start = "<!--"
        , end = "-->"
        , isNestable = False
        , isEscapable = False
        }
        |> source
        |> map ((,) Comment)


whitespace : Parser Syntax
whitespace =
    ignore oneOrMore isWhitespace
        |> source
        |> map ((,) Normal)


end : List Syntax -> Parser (List Syntax)
end revSyntaxList =
    Parser.end
        |> map (\_ -> List.reverse revSyntaxList)


syntaxToFragment : Syntax -> Fragment
syntaxToFragment ( syntaxType, text ) =
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
