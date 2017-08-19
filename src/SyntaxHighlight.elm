module SyntaxHighlight exposing (toHtml, highlightLines, elm, xml, javascript)

{-| Syntax highlighting in Elm.

@docs toHtml, highlightLines


## Languages

@docs elm, xml, javascript

-}

import Html exposing (Html)
import Parser
import SyntaxHighlight.Line as Line exposing (Line, Highlight)
import SyntaxHighlight.View as View
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Xml as Xml
import SyntaxHighlight.Language.Javascript as Javascript


{-| Transform to Html.
-}
toHtml : List Line -> Html msg
toHtml =
    View.toHtml


{-| Highlight lines given a start and end index.
Negative indexes are taken starting from the *end* of the list.
-}
highlightLines : Highlight -> Int -> Int -> List Line -> List Line
highlightLines =
    Line.highlightLines


{-| Highlight Elm syntax.
-}
elm : String -> Result Parser.Error (List Line)
elm =
    Elm.parse


{-| Highlight XML syntax.
-}
xml : String -> Result Parser.Error (List Line)
xml =
    Xml.parse


{-| Highlight Javascript syntax.
-}
javascript : String -> Result Parser.Error (List Line)
javascript =
    Javascript.parse
