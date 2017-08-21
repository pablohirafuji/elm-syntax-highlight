module SyntaxHighlight
    exposing
        ( toBlockHtml
        , toInlineHtml
        , elm
        , xml
        , javascript
        , Theme
        , useTheme
        , monokai
        , github
        )

{-| Syntax highlighting in Elm.

@docs toBlockHtml, toInlineHtml


## Languages

@docs elm, xml, javascript


## Themes

@docs Theme, useTheme, monokai, github

-}

import Html exposing (Html, text)
import Parser
import SyntaxHighlight.Line as Line exposing (Line, Highlight)
import SyntaxHighlight.View as View
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Xml as Xml
import SyntaxHighlight.Language.Javascript as Javascript
import SyntaxHighlight.Theme as Theme


{-| Transform a list of lines into a Html block. The `Maybe Int`
argument is for showing or not line count and, if so, starting
from what number.
-}
toBlockHtml : Maybe Int -> List Line -> Html msg
toBlockHtml =
    View.toBlockHtml


{-| Transform a list of lines into inline Html.
-}
toInlineHtml : List Line -> Html msg
toInlineHtml =
    View.toInlineHtml


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


{-| A theme defines the background and syntax colors.
-}
type Theme
    = Theme String


{-| Transform a theme into Html. Any highlighted code
will be themed according to the chosen `Theme`.
If you prefer to use CSS external stylesheet, you do **not** need this,
just copy the theme CSS into your stylesheet.
-}
useTheme : Theme -> Html msg
useTheme (Theme theme) =
    Html.node "style" [] [ text theme ]


{-| Monokai inspired theme. CSS link.
-}
monokai : Theme
monokai =
    Theme Theme.monokai


{-| GitHub inspired theme. CSS link.
-}
github : Theme
github =
    Theme Theme.github
