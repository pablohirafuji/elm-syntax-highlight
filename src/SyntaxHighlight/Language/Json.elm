module SyntaxHighlight.Language.Json exposing
    ( Syntax(..)
    ,  syntaxToStyle
       -- Exposing for tests purpose

    , toLines
    , toRevTokens
    )

import Parser exposing ((|.), DeadEnd, Parser, Step(..), andThen, chompIf, getChompedString, keyword, loop, map, oneOf, succeed, symbol)
import Set exposing (Set)
import SyntaxHighlight.Language.Helpers exposing (Delimiter, chompIfThenWhile, delimited, isEscapable, isLineBreak, isSpace, isWhitespace, thenChompWhile, whitespaceCharSet)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = String
    | Escapable
    | Number
    | Boolean
    | Null
    | Object
    | Array


toLines : String -> Result (List DeadEnd) (List Line)
toLines =
    Parser.run toRevTokens
        >> Result.map (Line.toLines syntaxToStyle)


toRevTokens : Parser (List Token)
toRevTokens =
    loop [] mainLoop


mainLoop : List Token -> Parser (Step (List Token) (List Token))
mainLoop revTokens =
    oneOf
        [ space
            |> map (\n -> Loop (n :: revTokens))
        , lineBreak
            |> map (\n -> Loop (n ++ revTokens))
        , stringLiteral revTokens
            |> map Loop
        , number
            |> map (\n -> Loop (n :: revTokens))
        , oneOf
            [ symbol "{"
            , symbol "}"
            , symbol ":"
            ]
            |> getChompedString
            |> map (\s -> ( T.C Object, s ))
            |> map (\n -> Loop (n :: revTokens))
        , oneOf
            [ symbol "["
            , symbol "]"
            ]
            |> getChompedString
            |> map (\s -> ( T.C Array, s ))
            |> map (\n -> Loop (n :: revTokens))
        , keyword "null"
            |> getChompedString
            |> map (\s -> ( T.C Null, s ))
            |> map (\n -> Loop (n :: revTokens))
        , oneOf
            [ keyword "true"
            , keyword "false"
            ]
            |> getChompedString
            |> map (\s -> ( T.C Boolean, s ))
            |> map (\n -> Loop (n :: revTokens))
        , chompIf (always True)
            |> getChompedString
            |> map (\b -> Loop (( T.Normal, b ) :: revTokens))
        , succeed (Done revTokens)
        ]



-- String literal


stringLiteral : List Token -> Parser (List Token)
stringLiteral revTokens =
    doubleQuote
        |> map (\n -> n ++ revTokens)


doubleQuote : Parser (List Token)
doubleQuote =
    delimited doubleQuoteDelimiter


doubleQuoteDelimiter : Delimiter Token
doubleQuoteDelimiter =
    { start = "\""
    , end = "\""
    , isNestable = False
    , defaultMap = \b -> ( T.C String, b )
    , innerParsers = [ lineBreak, stringEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }



-- Helpers


space : Parser Token
space =
    chompIfThenWhile isSpace
        |> getChompedString
        |> map (\b -> ( T.Normal, b ))


lineBreak : Parser (List Token)
lineBreak =
    symbol "\n"
        |> map (\_ -> [ ( T.LineBreak, "\n" ) ])


number : Parser Token
number =
    SyntaxHighlight.Language.Helpers.number
        |> getChompedString
        |> map (\b -> ( T.C Number, b ))


stringEscapable : Parser (List Token)
stringEscapable =
    escapable
        |> getChompedString
        |> map (\b -> [ ( T.C Escapable, b ) ])


escapable : Parser ()
escapable =
    succeed ()
        |. Parser.backtrackable (symbol "\\")
        |. chompIf isEscapableChar


isEscapableChar : Char -> Bool
isEscapableChar c =
    Set.member c escapableSet


escapableSet : Set Char
escapableSet =
    Set.fromList
        [ '"'
        , '\\'
        , '/'
        , 'b'
        , 'f'
        , 'n'
        , 'r'
        , 't'
        , 'u'
        ]


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        String ->
            ( Style2, "json-s" )

        Escapable ->
            ( Style1, "json-e" )

        Number ->
            ( Style1, "json-n" )

        Boolean ->
            ( Style3, "json-b" )

        Null ->
            ( Style3, "json-null" )

        Object ->
            ( Default, "json-o" )

        Array ->
            ( Default, "json-a" )
