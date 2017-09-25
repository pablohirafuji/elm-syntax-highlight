module SyntaxHighlight.Language.Helpers
    exposing
        ( isWhitespace
        , whitespaceCharSet
        , isSpace
        , isLineBreak
        , number
        , isNumber
        , Delimiter
        , delimited
        , thenIgnore
        , escapable
        , isEscapable
        , consThen
        , addThen
        )

import Set exposing (Set)
import Char
import Parser exposing (Parser, (|.), oneOf, keep, Count(..), oneOrMore, symbol, ignore, zeroOrMore, lazy, fail, source, map, andThen, delayedCommit)


isWhitespace : Char -> Bool
isWhitespace c =
    isSpace c || isLineBreak c


whitespaceCharSet : Set Char
whitespaceCharSet =
    Set.fromList
        [ ' '
        , '\t'
        , '\n'
        ]


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t'


isLineBreak : Char -> Bool
isLineBreak c =
    c == '\n'


number : Parser ()
number =
    oneOf
        [ positiveNumber
        , delayedCommit (symbol "-") positiveNumber
        ]


positiveNumber : Parser ()
positiveNumber =
    ignore oneOrMore isNumber


isNumber : Char -> Bool
isNumber c =
    Char.isDigit c || c == '.'



{- Delimiter

   When defining isNotRelevant, make sure to add all chars that
   innerParsers starts with.
-}


type alias Delimiter a =
    { start : String
    , end : String
    , isNestable : Bool
    , defaultMap : String -> a
    , innerParsers : List (Parser (List a))
    , isNotRelevant : Char -> Bool
    }


delimited : Delimiter a -> Parser (List a)
delimited ({ start, isNotRelevant, defaultMap } as options) =
    symbol start
        |> map (always (defaultMap start))
        |> andThen (\n -> delimitedHelp options [ n ])


delimitedHelp : Delimiter a -> List a -> Parser (List a)
delimitedHelp ({ start, end, isNotRelevant } as options) revAList =
    case ( String.uncons options.start, String.uncons options.end ) of
        ( Nothing, _ ) ->
            fail "Trying to parse a delimited helper, but the start token cannot be an empty string!"

        ( _, Nothing ) ->
            fail "Trying to parse a delimited helper, but the end token cannot be an empty string!"

        ( Just ( startChar, _ ), Just ( endChar, _ ) ) ->
            if options.isNestable then
                delimitedNestable 1
                    { options
                        | isNotRelevant =
                            \c -> isNotRelevant c && c /= startChar && c /= endChar
                    }
                    revAList
            else
                delimitedUnnestable
                    { options
                        | isNotRelevant =
                            \c -> isNotRelevant c && c /= endChar
                    }
                    revAList


delimitedUnnestable : Delimiter a -> List a -> Parser (List a)
delimitedUnnestable ({ defaultMap, isNotRelevant, end, innerParsers } as options) revAList =
    oneOf
        [ symbol end |> map (always (defaultMap end :: revAList))
        , Parser.end |> map (always revAList)
        , oneOf innerParsers
            |> addThen (delimitedUnnestable options) revAList
        , oneOf
            [ keep oneOrMore isNotRelevant |> map defaultMap
            , ignore (Exactly 1) (always True)
                |> thenIgnore zeroOrMore isNotRelevant
                |> source
                |> map defaultMap
            ]
            |> consThen (delimitedUnnestable options) revAList
        ]


delimitedNestable : Int -> Delimiter a -> List a -> Parser (List a)
delimitedNestable nestLevel ({ defaultMap, isNotRelevant, start, end, innerParsers } as options) revAList =
    oneOf
        [ symbol end
            |> map (always (defaultMap end :: revAList))
            |> andThen
                (\n ->
                    if nestLevel == 1 then
                        Parser.succeed n
                    else
                        delimitedNestable (nestLevel - 1) options n
                )
        , symbol start
            |> thenIgnore zeroOrMore isNotRelevant
            |> source
            |> map defaultMap
            |> consThen (delimitedNestable (nestLevel + 1) options) revAList
        , Parser.end |> map (always revAList)
        , oneOf innerParsers
            |> addThen (delimitedUnnestable options) revAList
        , oneOf
            [ keep oneOrMore isNotRelevant |> map defaultMap
            , ignore (Exactly 1) (always True)
                |> thenIgnore zeroOrMore isNotRelevant
                |> source
                |> map defaultMap
            ]
            |> consThen (delimitedNestable nestLevel options) revAList
        ]


thenIgnore : Parser.Count -> (Char -> Bool) -> Parser a -> Parser a
thenIgnore count isNotRelevant previousParser =
    previousParser
        |. ignore count isNotRelevant


consThen : (List a -> Parser (List a)) -> List a -> Parser a -> Parser (List a)
consThen f list pn =
    andThen (\n -> f (n :: list)) pn


addThen : (List a -> Parser (List a)) -> List a -> Parser (List a) -> Parser (List a)
addThen f list plist =
    andThen (\n -> f (n ++ list)) plist



-- Inner parser Helpers


escapable : Parser ()
escapable =
    Parser.delayedCommit (symbol "\\") <|
        ignore (Exactly 1) isEscapableChar


isEscapable : Char -> Bool
isEscapable c =
    c == '\\'


isEscapableChar : Char -> Bool
isEscapableChar c =
    Set.member c escapableSet


escapableSet : Set Char
escapableSet =
    Set.fromList
        [ '\''
        , '"'
        , '\\'
        , 'n'
        , 'r'
        , 't'
        , 'b'
        , 'f'
        , 'v'
        ]
