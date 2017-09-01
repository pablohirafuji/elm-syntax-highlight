module SyntaxHighlight
    exposing
        ( toBlockHtml
        , toInlineHtml
        , elm
        , xml
        , javascript
        , css
        , Theme
        , useTheme
        , monokai
        , github
        , oneDark
        )

{-| Syntax highlighting in Elm.

@docs toBlockHtml, toInlineHtml


## Languages

Error while parsing should not happen. If it happens, please [open an issue](https://github.com/pablohirafuji/elm-syntax-highlight/issues) with the code that gives the error and the language.

@docs elm, xml, javascript, css


## Themes

@docs Theme, useTheme, monokai, github, oneDark

-}

import Html exposing (Html, text)
import Parser
import SyntaxHighlight.Line as Line exposing (Line, Highlight)
import SyntaxHighlight.View as View
import SyntaxHighlight.Language.Elm as Elm
import SyntaxHighlight.Language.Xml as Xml
import SyntaxHighlight.Language.Javascript as Javascript
import SyntaxHighlight.Language.Css as Css
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


{-| Parse Elm syntax.
-}
elm : String -> Result Parser.Error (List Line)
elm =
    Elm.parse


{-| Parse XML syntax.
-}
xml : String -> Result Parser.Error (List Line)
xml =
    Xml.parse


{-| Parse Javascript syntax.
-}
javascript : String -> Result Parser.Error (List Line)
javascript =
    Javascript.parse


{-| Parse CSS syntax.
-}
css : String -> Result Parser.Error (List Line)
css =
    Css.parse


{-| A theme defines the background and syntax colors.
-}
type Theme
    = Theme String


{-| Transform a theme into Html. Any highlighted code
will be themed according to the chosen `Theme`.

To preview the themes, check out the [demo](https://pablohirafuji.github.io/elm-syntax-highlight/).

If you prefer to use CSS external stylesheet, you do **not** need this,
just copy the theme CSS into your stylesheet.
All themes can be found [here](https://github.com/pablohirafuji/elm-syntax-highlight/blob/master/themes.md).

-}
useTheme : Theme -> Html msg
useTheme (Theme theme) =
    Html.node "style" [] [ text theme ]


{-| Monokai inspired theme.
-}
monokai : Theme
monokai =
    Theme Theme.monokai


{-| GitHub inspired theme.
-}
github : Theme
github =
    Theme Theme.github


{-| Atom One Dark inspired theme.
-}
oneDark : Theme
oneDark =
    Theme Theme.oneDark
