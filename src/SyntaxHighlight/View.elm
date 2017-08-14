module SyntaxHighlight.View exposing (toHtml)

import Html exposing (Html, text, span, br, code)
import Html.Attributes exposing (classList)
import SyntaxHighlight.Fragment exposing (..)


toHtml : List Fragment -> Html msg
toHtml fragment =
    fragment
        |> List.map toElement
        |> code []


toElement : Fragment -> Html msg
toElement { color, isEmphasis, isStrong, text } =
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
