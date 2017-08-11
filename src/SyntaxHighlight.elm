module SyntaxHighlight exposing (elm, xml, javascript)

{-| Syntax highlighting in Elm.

@docs elm, xml, javascript

-}

import Html exposing (Html)
import SyntaxHighlight.View as View
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Xml as Xml
import SyntaxHighlight.Language.Javascript as Javascript


{-| Highlight Elm syntax.
-}
elm : String -> Html msg
elm =
    Elm.parse
        >> View.toHtml


{-| Highlight XML syntax.
-}
xml : String -> Html msg
xml =
    Xml.parse
        >> View.toHtml


{-| Highlight Javascript syntax.
-}
javascript : String -> Html msg
javascript =
    Javascript.parse
        >> View.toHtml
