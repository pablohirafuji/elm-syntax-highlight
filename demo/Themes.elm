module Themes exposing (view)

import Html exposing (..)
import Html.Attributes exposing (attribute, id, href, class)
import SyntaxHighlight.Theme as Theme


main : Html msg
main =
    view


view : Html msg
view =
    node "html"
        []
        [ headerView
        , bodyView
        ]


headerView : Html msg
headerView =
    node "head"
        []
        [ node "meta" [ attribute "charset" "utf-8" ] []
        , node "meta"
            [ attribute "name" "viewport"
            , attribute "content" "width=device-width, initial-scale=1"
            ]
            []
        , node "title" [] [ text "Elm Syntax Highlight - Themes" ]
        , node "style" [] [ text styleView ]
        ]


styleView : String
styleView =
    """body {
    margin: 40px auto;
    max-width: 650px;
    line-height: 1.6;
    font-size: 18px;
    color: #444;
    padding: 0 10px;
}
h1,h2,h3 {
    line-height: 1.2
}
h1 {
    text-align: center;
    padding-bottom: 0;
    margin-bottom: 0;
}
.subheading {
    margin-top: 0;
    text-align: center;
}"""


bodyView : Html msg
bodyView =
    body []
        [ h1 [] [ text "Elm Syntax Highlight Themes" ]
        , p [ class "subheading" ]
            [ a [ href "http://package.elm-lang.org/packages/pablohirafuji/elm-syntax-highlight/latest" ] [ text "Package" ]
            , text " / "
            , a [ href "https://github.com/pablohirafuji/elm-syntax-highlight" ] [ text "GitHub" ]
            , text " / "
            , a [ href "https://pablohirafuji.github.io/elm-syntax-highlight/" ] [ text "Demo" ]
            ]
        , tableOfContent
        , h2 [ id "Required-Styles" ] [ text "Required Styles" ]
        , requiredStylesView
        , themesView
        ]


tableOfContent : Html msg
tableOfContent =
    List.map (Tuple.first >> tableOfContentHelper) Theme.all
        |> (::) (li [] [ a [ href "#Required-Styles" ] [ text "Required Styles" ] ])
        |> ul []


tableOfContentHelper : String -> Html msg
tableOfContentHelper name =
    li [] [ a [ href ("#" ++ nameToId name) ] [ text name ] ]


requiredStylesView : Html msg
requiredStylesView =
    pre [] [ code [] [ text requiredStyles ] ]


requiredStyles : String
requiredStyles =
    """
pre.elmsh {
    padding: 10px;
    margin: 0;
    text-align: left;
    overflow: auto;
}
code.elmsh {
    padding: 0;
}
.elmsh-line:before {
    content: attr(data-elmsh-lc);
    display: inline-block;
    text-align: right;
    width: 40px;
    padding: 0 20px 0 0;
    opacity: 0.3;
}"""


themesView : Html msg
themesView =
    div [] <|
        List.map themeView Theme.all


themeView : ( String, String ) -> Html msg
themeView ( name, css ) =
    div []
        [ h2 [ id (nameToId name) ] [ text name ]
        , pre [] [ code [] [ text css ] ]
        ]


nameToId : String -> String
nameToId =
    String.map
        (\c ->
            if c == ' ' then
                '-'
            else
                c
        )
