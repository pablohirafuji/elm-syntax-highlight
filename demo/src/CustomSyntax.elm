module CustomSyntax exposing (syntax)

import Parser exposing ((|.), (|=), Parser)
import SyntaxHighlight exposing (HCode)
import SyntaxHighlight.Custom as Custom exposing (Fragment, Style)


type Token
    = Number String
    | Operator String
    | Parenthesis String
    | Other String


syntax : String -> Result (List Parser.DeadEnd) HCode
syntax =
    Custom.fromParser parser


parser : Parser (List Fragment)
parser =
    Parser.loop [] parserLoop
        |> Parser.map (List.map tokenToFragment)


parserLoop : List Token -> Parser (Parser.Step (List Token) (List Token))
parserLoop revTokens =
    Parser.oneOf
        [ Parser.end
            |> Parser.map (\() -> Parser.Done (List.reverse revTokens))
        , Parser.oneOf [ number, operator, parenthesis, other ]
            |> Parser.map (\token -> Parser.Loop (token :: revTokens))
        ]


tokenToFragment : Token -> Fragment
tokenToFragment token =
    case token of
        Number text ->
            fragment text Custom.style1

        Operator text ->
            fragment text Custom.style3

        Parenthesis text ->
            fragment text Custom.style4

        Other text ->
            fragment text Custom.styleDefault


fragment : String -> Style -> Fragment
fragment text style =
    Custom.fragment text |> Custom.setFragmentStyle style



-- TOKEN PARSING


number : Parser Token
number =
    chars isNumber
        |> Parser.map Number


isNumber : Char -> Bool
isNumber =
    Char.isDigit


operator : Parser Token
operator =
    chars isOperator
        |> Parser.map Operator


isOperator : Char -> Bool
isOperator char =
    char == '+' || char == '-' || char == '*' || char == '/'


parenthesis : Parser Token
parenthesis =
    chars isParenthesis
        |> Parser.map Parenthesis


isParenthesis : Char -> Bool
isParenthesis char =
    char == '(' || char == ')'


other : Parser Token
other =
    chars
        (\char ->
            not (isNumber char)
                && not (isOperator char)
                && not (isParenthesis char)
        )
        |> Parser.map Other


chars : (Char -> Bool) -> Parser String
chars isGood =
    Parser.succeed ()
        |. Parser.chompIf isGood
        |. Parser.chompWhile isGood
        |> Parser.getChompedString
