module CustomSyntax exposing (syntax)

import Parser exposing ((|.), (|=), Parser)
import SyntaxHighlight exposing (HCode)
import SyntaxHighlight.Custom as SH


type Token
    = Number String
    | Operator String
    | Parenthesis String
    | LineBreak
    | Other String


syntax : String -> Result (List Parser.DeadEnd) HCode
syntax =
    SH.fromParser parser


parser : Parser (List SH.Line)
parser =
    Parser.loop [] parserLoop
        |> Parser.map tokensToLines


parserLoop : List Token -> Parser (Parser.Step (List Token) (List Token))
parserLoop revTokens =
    Parser.oneOf
        [ Parser.end
            |> Parser.map (\() -> Parser.Done (List.reverse revTokens))
        , Parser.oneOf [ lineBreak, number, operator, parenthesis, other ]
            |> Parser.map (\token -> Parser.Loop (token :: revTokens))
        ]


tokensToLines : List Token -> List SH.Line
tokensToLines tokens =
    let
        accumulateLines : Token -> ( List SH.Fragment, List SH.Line ) -> ( List SH.Fragment, List SH.Line )
        accumulateLines token ( accFragmentsRev, linesRev ) =
            let
                addFragment text style =
                    ( (SH.fragment text |> SH.setFragmentStyle style)
                        :: accFragmentsRev
                    , linesRev
                    )
            in
            case token of
                LineBreak ->
                    ( []
                    , SH.line (List.reverse accFragmentsRev) :: linesRev
                    )

                Number text ->
                    addFragment text SH.style1

                Operator text ->
                    addFragment text SH.style3

                Parenthesis text ->
                    addFragment text SH.style4

                Other text ->
                    addFragment text SH.styleDefault

        ( lastLineFragmentsRev, otherLinesRev ) =
            tokens
                |> List.foldl accumulateLines ( [], [] )

        lastLine =
            SH.line (List.reverse lastLineFragmentsRev)
    in
    List.reverse (lastLine :: otherLinesRev)



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
