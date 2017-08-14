module SyntaxHighlight exposing (toHtml, elm, xml, javascript)

{-| Syntax highlighting in Elm.

@docs toHtml


## Languages

@docs elm, xml, javascript

-}

import Html exposing (Html)
import Parser
import SyntaxHighlight.Fragment exposing (Fragment)
import SyntaxHighlight.View as View
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Xml as Xml
import SyntaxHighlight.Language.Javascript as Javascript


{-| Transform to Html.
-}
toHtml : List Fragment -> Html msg
toHtml =
    View.toHtml


{-| Highlight Elm syntax.
-}
elm : String -> Result Parser.Error (List Fragment)
elm =
    Elm.parse


{-| Highlight XML syntax.
-}
xml : String -> Result Parser.Error (List Fragment)
xml =
    Xml.parse


{-| Highlight Javascript syntax.
-}
javascript : String -> Result Parser.Error (List Fragment)
javascript =
    Javascript.parse
