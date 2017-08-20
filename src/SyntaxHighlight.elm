module SyntaxHighlight
    exposing
        ( toHtml
        , highlightLines
        , elm
        , xml
        , javascript
        , Theme
        , useTheme
        , monokai
        , github
        )

{-| Syntax highlighting in Elm.

@docs toHtml, highlightLines


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


{-| Transform a list of lines into Html.
-}
toHtml : List Line -> Html msg
toHtml =
    View.toHtml


{-| Highlight lines given a start and end index.
Negative indexes are taken starting from the *end* of the list.
-}
highlightLines : Maybe Highlight -> Int -> Int -> List Line -> List Line
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


{-| Theme type.
-}
type Theme
    = Theme String


{-| Transform a theme into Html. Any highlighted code
will be themed according to the chosen `Theme`.
The `Bool` argument is for showing or not a line counter.
-}
useTheme : Bool -> Theme -> Html msg
useTheme showLineCount (Theme theme) =
    let
        style =
            if showLineCount then
                showLineCountCSS ++ theme
            else
                theme
    in
        Html.node "style" [] [ text style ]


showLineCountCSS : String
showLineCountCSS =
    """
.elmsh {
    counter-reset: line;
}
.elmsh-line:before {
    counter-increment: line;
    content: counter(line);
    display: inline-block;
    text-align: right;
    width: 40px;
    padding: 0 20px 0 0;
    color: #75715E;
}"""


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
