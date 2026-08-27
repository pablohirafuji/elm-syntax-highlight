module Custom exposing (parser)

import Parser exposing ((|.), (|=), Parser)
import SyntaxHighlight.Custom as Sh


type Token
    = Number String
    | Operator String
    | Parenthesis String
    | LineBreak
    | Other String


parser : Parser (List Sh.Line)
parser =
    Parser.loop [] parserLoop
        |> Parser.map
            (\tokens ->
                let
                    ( lastLineFragmentsRev, otherLinesRev ) =
                        tokens
                            |> List.foldl
                                (\token ( accFragmentsRev, linesRev ) ->
                                    let
                                        addFragment text style =
                                            ( (Sh.fragment text |> Sh.setFragmentStyle style)
                                                :: accFragmentsRev
                                            , linesRev
                                            )
                                    in
                                    case token of
                                        LineBreak ->
                                            ( []
                                            , Sh.line (List.reverse accFragmentsRev) :: linesRev
                                            )

                                        Number text ->
                                            addFragment text Sh.style1

                                        Operator text ->
                                            addFragment text Sh.style2

                                        Parenthesis text ->
                                            addFragment text Sh.style3

                                        Other text ->
                                            addFragment text Sh.styleDefault
                                )
                                ( [], [] )
                in
                List.reverse (Sh.line (List.reverse lastLineFragmentsRev) :: otherLinesRev)
            )


parserLoop : List Token -> Parser (Parser.Step (List Token) (List Token))
parserLoop revTokens =
    Parser.oneOf
        [ Parser.end
            |> Parser.map (\() -> Parser.Done (List.reverse revTokens))
        , Parser.oneOf [ lineBreak, number, operator, parenthesis, other ]
            |> Parser.map (\token -> Parser.Loop (token :: revTokens))
        ]



-- TOKEN PARSERS


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


lineBreak : Parser Token
lineBreak =
    Parser.succeed LineBreak
        |. Parser.chompIf isLineBreak


isLineBreak : Char -> Bool
isLineBreak char =
    char == '\n'


other : Parser Token
other =
    chars
        (\char ->
            not (isNumber char)
                && not (isOperator char)
                && not (isParenthesis char)
                && not (isLineBreak char)
        )
        |> Parser.map Other


chars : (Char -> Bool) -> Parser String
chars isGood =
    Parser.succeed ()
        |. Parser.chompIf isGood
        |. Parser.chompWhile isGood
        |> Parser.getChompedString
