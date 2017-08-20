module SyntaxHighlight.View exposing (toHtml)

import Html exposing (Html, text, span, br, code, div)
import Html.Attributes exposing (class, classList)
import SyntaxHighlight.Line exposing (..)


toHtml : List Line -> Html msg
toHtml lines =
    lines
        |> List.map lineView
        |> code [ class "elmsh" ]


lineView : Line -> Html msg
lineView { fragments, highlight } =
    fragments
        |> List.map elementView
        |> div
            [ classList
                [ ( "elmsh-line", True )
                , ( "elmsh-hl", highlight == Just Normal )
                , ( "elmsh-add", highlight == Just Add )
                , ( "elmsh-del", highlight == Just Delete )
                ]
            ]


elementView : Fragment -> Html msg
elementView { text, color, isEmphasis, isStrong } =
    if color == Default && not isEmphasis && not isStrong then
        Html.text text
    else
        span
            [ classList
                [ ( colorToString color, color /= Default )
                , ( "elmsh-emphasis", isEmphasis )
                , ( "elmsh-strong", isStrong )
                ]
            ]
            [ Html.text text ]


colorToString : Color -> String
colorToString color =
    (++) "elmsh" <|
        case color of
            Default ->
                "0"

            Color1 ->
                "1"

            Color2 ->
                "2"

            Color3 ->
                "3"

            Color4 ->
                "4"

            Color5 ->
                "5"

            Color6 ->
                "6"

            Color7 ->
                "7"
