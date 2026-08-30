module Custom exposing (suite)

import Expect exposing (equal, fail)
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
            { givenParserOutput =
                [ Custom.newline (Just Add)
                , Custom.fragment "code"
                ]
            , expectHCodeLines =
                [ { fragments = [ { emptyFragment | text = "code" } ]
                  , highlight = Just Line.Add
                  }
                ]
            }
        , testFromParsedFragments "Doesn't leave empty fragments in empty lines"
            { givenParserOutput =
                [ Custom.fragment "before\n\nafter" ]
            , expectHCodeLines =
                [ { emptyLine | fragments = [ { emptyFragment | text = "before" } ] }
                , { emptyLine | fragments = [] }
                , { emptyLine | fragments = [ { emptyFragment | text = "after" } ] }
                ]
            }
        ]


{-| Makes a test by constructing a parser that simply outputs the provided
`givenParserOutput` fragments, and checks the output of `fromParser` against
`expectedHCodeLines`.
-}
testFromParsedFragments :
    String
    ->
        { givenParserOutput : List Custom.Fragment
        , expectHCodeLines : List Line
        }
    -> Test
testFromParsedFragments testName config =
    test testName <|
        \_ ->
            case Custom.fromParser (Parser.succeed config.givenParserOutput) "" of
                Ok hCode ->
                    equal hCode (SyntaxHighlight.toHCode config.expectHCodeLines)

                _ ->
                    fail "`fromParser` returned an unexpected value"


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
