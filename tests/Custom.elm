module Custom exposing (suite)

import Expect exposing (Expectation, equal, fail)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight exposing (HCode, Highlight(..))
import SyntaxHighlight.Custom as Custom
import SyntaxHighlight.Line as Line exposing (Line)
import SyntaxHighlight.Style as Style
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Custom Syntax Test Suite"
        [ testFromParsedFragments "First line highlighting doesn't add an extra empty line"
            [ Custom.newline (Just Add)
            , Custom.fragment "code"
            ]
            [ { fragments = [ { emptyFragment | text = "code" } ]
              , highlight = Just Line.Add
              }
            ]
        , testFromParsedFragments "Doesn't leave empty fragments in empty lines"
            [ Custom.fragment "before\n\nafter" ]
            [ { emptyLine | fragments = [ { emptyFragment | text = "before" } ] }
            , { emptyLine | fragments = [] }
            , { emptyLine | fragments = [ { emptyFragment | text = "after" } ] }
            ]
        ]


testFromParsedFragments : String -> List Custom.Fragment -> List Line -> Test
testFromParsedFragments testName fragments resultMatchesLines =
    test testName <|
        \_ ->
            case fromParsedFragments fragments of
                Ok hCode ->
                    equal hCode (SyntaxHighlight.toHCode resultMatchesLines)

                _ ->
                    fail "`fromParsedFragments` returned an unexpected value"


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
