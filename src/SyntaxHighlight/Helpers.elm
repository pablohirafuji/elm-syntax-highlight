module SyntaxHighlight.Helpers exposing (..)

import Char
import Parser exposing (Parser, (|.), oneOf, keep, Count(..), oneOrMore, symbol, ignore, zeroOrMore, lazy, fail)


isWhitespace : Char -> Bool
isWhitespace c =
    isSpace c || isLineBreak c


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\t'


isLineBreak : Char -> Bool
isLineBreak c =
    c == '\n'


number : Parser ()
number =
    ignore oneOrMore isNumber


isNumber : Char -> Bool
isNumber c =
    Char.isDigit c || c == '.'


type alias Delimited =
    { start : String
    , end : String
    , isNestable : Bool
    , isEscapable : Bool
    }


delimited : Delimited -> Parser ()
delimited ({ start, end, isNestable, isEscapable } as options) =
    case ( String.uncons start, String.uncons end ) of
        ( Nothing, _ ) ->
            fail "Trying to parse a delimited helper, but the start token cannot be an empty string!"

        ( _, Nothing ) ->
            fail "Trying to parse a delimited helper, but the end token cannot be an empty string!"

        ( Just ( startChar, _ ), Just ( endChar, _ ) ) ->
            symbol options.start
                |. delimitedHelp options startChar endChar


delimitedHelp : Delimited -> Char -> Char -> Parser ()
delimitedHelp options startChar endChar =
    case ( options.isNestable, options.isEscapable ) of
        ( False, False ) ->
            let
                isNotRelevant char =
                    char /= endChar
            in
                unnestableDelimited isNotRelevant options endChar

        ( False, True ) ->
            let
                isNotRelevant char =
                    char /= '\\' && char /= endChar
            in
                unnestableDelimited isNotRelevant options endChar

        ( True, False ) ->
            let
                isNotRelevant char =
                    char /= startChar && char /= endChar
            in
                nestableDelimited isNotRelevant options startChar endChar 1

        ( True, True ) ->
            let
                isNotRelevant char =
                    char /= '\\' && char /= startChar || char /= endChar
            in
                nestableDelimited isNotRelevant options startChar endChar 1


unnestableDelimited : (Char -> Bool) -> Delimited -> Char -> Parser ()
unnestableDelimited isNotRelevant options endChar =
    lazy <|
        \_ ->
            ignore zeroOrMore isNotRelevant
                |. oneOf
                    ([ symbol options.end
                     , Parser.end
                     , ignore (Exactly 1) ((==) endChar)
                        |. unnestableDelimited isNotRelevant options endChar
                     ]
                        ++ (if options.isEscapable then
                                [ ignore (Exactly 1) ((==) '\\')
                                    |. ignore (Exactly 1) (always True)
                                    |. unnestableDelimited isNotRelevant options endChar
                                ]
                            else
                                []
                           )
                    )


nestableDelimited : (Char -> Bool) -> Delimited -> Char -> Char -> Int -> Parser ()
nestableDelimited isNotRelevant ({ start, end } as options) startChar endChar nestLevel =
    lazy <|
        \_ ->
            ignore zeroOrMore isNotRelevant
                |. oneOf
                    ([ (if nestLevel == 1 then
                            symbol end
                        else
                            symbol end
                                |. nestableDelimited isNotRelevant options startChar endChar (nestLevel - 1)
                       )
                     , symbol start
                        |. nestableDelimited isNotRelevant options startChar endChar (nestLevel + 1)
                     , Parser.end
                     , ignore (Exactly 1) (\c -> c == startChar || c == endChar)
                        |. nestableDelimited isNotRelevant options startChar endChar nestLevel
                     ]
                        ++ (if options.isEscapable then
                                [ ignore (Exactly 1) ((==) '\\')
                                    |. ignore (Exactly 1) (always True)
                                    |. nestableDelimited isNotRelevant options startChar endChar nestLevel
                                ]
                            else
                                []
                           )
                    )
