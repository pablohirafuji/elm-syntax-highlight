module SyntaxHighlight.View exposing (toHtml)

import Html exposing (Html, text, span, br, code, div)
import Html.Attributes exposing (classList)
import SyntaxHighlight.Line exposing (..)


toHtml : List Line -> Html msg
toHtml lines =
    lines
        |> List.map lineView
        |> code []


lineView : Line -> Html msg
lineView { fragments, highlight } =
    fragments
        |> List.map elementView
        |> div
            [ classList
                [ ( "elmshLine", True )
                , ( "elmshHighlight", highlight == Just Normal )
                , ( "elmshAdd", highlight == Just Add )
                , ( "elmshDel", highlight == Just Delete )
                ]
            ]


elementView : Fragment -> Html msg
elementView { text, color, isEmphasis, isStrong } =
    if color == Default && not isEmphasis && not isStrong then
        Html.text text
    else
        span
            [ classList
                [ ( "elmsh" ++ toString color, color /= Default )
                , ( "elmshEmphasis", isEmphasis )
                , ( "elmshStrong", isStrong )
                ]
            ]
            [ Html.text text ]
