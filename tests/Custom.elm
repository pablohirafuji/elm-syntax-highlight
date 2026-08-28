module Custom exposing (suite)

import Expect exposing (Expectation, equal, fail)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight exposing (Highlight(..))
import SyntaxHighlight.Custom as Custom
import SyntaxHighlight.Language.Type exposing (HCode(..))
import SyntaxHighlight.Line as Line exposing (Line)
import SyntaxHighlight.Style as Style
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Custom Syntax Test Suite"
        [ testFromParsedFragments "First line highlighting doesn't add an extra empty line"
            [ Custom.lineBreak (Just Add)
            , Custom.fragment "code"
            ]
            (equal
                [ { fragments = [ { emptyFragment | text = "code" } ]
                  , highlight = Just Line.Add
                  }
                ]
            )
        , testFromParsedFragments "Doesn't leave empty fragments in empty lines"
            [ Custom.fragment "before\n\nafter" ]
            (equal
                [ { emptyLine | fragments = [ { emptyFragment | text = "before" } ] }
                , { emptyLine | fragments = [] }
                , { emptyLine | fragments = [ { emptyFragment | text = "after" } ] }
                ]
            )
        ]


testFromParsedFragments : String -> List Custom.Fragment -> (List Line -> Expectation) -> Test
testFromParsedFragments testName fragments checkLines =
    test testName <|
        \_ ->
            case fromParsedFragments fragments of
                Ok (HCode lines) ->
                    checkLines lines

                _ ->
                    fail "TODO"


fromParsedFragments : List Custom.Fragment -> Result (List Parser.DeadEnd) HCode
fromParsedFragments fragments =
    Custom.fromParser (Parser.succeed fragments) ""


emptyLine : Line
emptyLine =
    { fragments = []
    , highlight = Nothing
    }


emptyFragment : Line.Fragment
emptyFragment =
    { text = ""
    , requiredStyle = Style.Default
    , additionalClass = ""
    }
