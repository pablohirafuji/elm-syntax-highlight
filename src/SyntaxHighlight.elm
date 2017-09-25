module SyntaxHighlight
    exposing
        ( toBlockHtml
        , toInlineHtml
        , toStaticBlockHtml
        , toStaticInlineHtml
        , elm
        , xml
        , javascript
        , css
        , Theme
        , useTheme
        , monokai
        , gitHub
        , oneDark
        )

{-| Syntax highlighting in Elm.

@docs toBlockHtml, toInlineHtml, toStaticBlockHtml, toStaticInlineHtml


## Languages

Error while parsing should not happen. If it happens, please [open an issue](https://github.com/pablohirafuji/elm-syntax-highlight/issues) with the code that gives the error and the language.

@docs elm, xml, javascript, css


## Themes

@docs Theme, useTheme, monokai, gitHub, oneDark

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


{-| Transform a list of lines into a Html block.
The `Maybe Int` argument is for showing or not line count and, if so, starting from what number.
-}
toBlockHtml : Maybe Int -> List Line -> Html msg
toBlockHtml =
    View.toBlockHtml


{-| Transform a list of lines into inline Html.

    import SyntaxHighlight exposing (elm, toInlineHtml)

    info : Html msg
    info =
        p []
            [ text "This function signature "
            , elm "isEmpty : String -> Bool"
                |> Result.map toInlineHtml
                |> Result.withDefault
                    (code [] [ text "isEmpty : String -> Bool" ])
            , text " means that a String argument is taken, then a Bool is returned."
            ]

-}
toInlineHtml : List Line -> Html msg
toInlineHtml =
    View.toInlineHtml


{-| Transform a list of lines into a static (pure text) Html block. The `Maybe Int` argument is for showing or not line count and, if so, starting from what number.
-}
toStaticBlockHtml : Maybe Int -> List Line -> String
toStaticBlockHtml =
    View.toStaticBlockHtml


{-| Transform a list of lines into static (pure text) inline Html.
-}
toStaticInlineHtml : List Line -> String
toStaticInlineHtml =
    View.toStaticInlineHtml


{-| Parse Elm syntax.
-}
elm : String -> Result Parser.Error (List Line)
elm =
    Elm.toLines


{-| Parse XML syntax.
-}
xml : String -> Result Parser.Error (List Line)
xml =
    Xml.toLines


{-| Parse Javascript syntax.
-}
javascript : String -> Result Parser.Error (List Line)
javascript =
    Javascript.toLines


{-| Parse CSS syntax.
-}
css : String -> Result Parser.Error (List Line)
css =
    Css.toLines


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

    import SyntaxHighlight exposing (useTheme, monokai, elm, toBlockHtml)

    view : Model -> Html msg
    view model =
        div []
            [ useTheme monokai
            , elm model.elmCode
                |> Result.map (toBlockHtml (Just 1))
                |> Result.withDefault
                    (pre [] [ code [] [ text model.elmCode ] ])
            ]

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
gitHub : Theme
gitHub =
    Theme Theme.gitHub


{-| Atom One Dark inspired theme.
-}
oneDark : Theme
oneDark =
    Theme Theme.oneDark
