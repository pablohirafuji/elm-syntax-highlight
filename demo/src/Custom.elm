module Custom exposing (parser)

import Parser exposing ((|.), (|=), Parser)
import SyntaxHighlight.Custom as Sh


type alias Token =
    ( TokenKind, String )


type TokenKind
    = Whitespace
    | Number
    | Operator


parser : Parser (List Sh.Line)
parser =
    Parser.loop [] parserLoop
        |> Parser.map
            (\revTokens ->
                List.reverse revTokens
                    |> List.map tokenToFragment
                    |> Sh.line
                    |> List.singleton
            )


parserLoop : List Token -> Parser (Parser.Step (List Token) (List Token))
parserLoop revTokens =
    Parser.oneOf
        [ Parser.end
            |> Parser.map (\() -> Parser.Done revTokens)
        , Parser.oneOf [ number, operator, whitespace ]
            |> Parser.map (\token -> Parser.Loop (token :: revTokens))
        ]


tokenToFragment : Token -> Sh.Fragment
tokenToFragment ( kind, text ) =
    let
        style =
            case kind of
                Whitespace ->
                    Sh.styleDefault

                Number ->
                    Sh.style6

                Operator ->
                    Sh.style3
    in
    Sh.fragment text
        |> Sh.setFragmentStyle style



-- TOKEN PARSERS


number : Parser Token
number =
    chars Char.isDigit
        |> Parser.map (\s -> ( Number, s ))


operator : Parser Token
operator =
    chars (\char -> char == '+' || char == '-' || char == '*' || char == '/')
        |> Parser.map (\s -> ( Operator, s ))


whitespace : Parser Token
whitespace =
    chars (\char -> char == ' ')
        |> Parser.map (\s -> ( Whitespace, s ))


chars : (Char -> Bool) -> Parser String
chars isGood =
    Parser.succeed ()
        |. Parser.chompIf isGood
        |. Parser.chompWhile isGood
        |> Parser.getChompedString
