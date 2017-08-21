module SyntaxHighlight.View exposing (toBlockHtml, toInlineHtml)

import Html exposing (Html, text, span, br, code, div, pre)
import Html.Attributes exposing (class, classList, attribute)
import SyntaxHighlight.Line exposing (..)


toBlockHtml : Maybe Int -> List Line -> Html msg
toBlockHtml maybeStart lines =
    case maybeStart of
        Nothing ->
            pre [ class "elmsh" ]
                [ toInlineHtml lines ]

        Just start ->
            lines
                |> List.indexedMap (lineView start)
                |> code []
                |> List.singleton
                |> pre [ class "elmsh" ]


lineView : Int -> Int -> Line -> Html msg
lineView start index { fragments, highlight } =
    div
        [ classList
            [ ( "elmsh-line", True )
            , ( "elmsh-hl", highlight == Just Normal )
            , ( "elmsh-add", highlight == Just Add )
            , ( "elmsh-del", highlight == Just Delete )
            ]
        , attribute "data-elmsh-lc" (toString (start + index))
        ]
        (List.map elementView fragments)


toInlineHtml : List Line -> Html msg
toInlineHtml lines =
    lines
        |> List.map
            (\{ highlight, fragments } ->
                if highlight == Nothing then
                    List.map elementView fragments
                else
                    [ span
                        [ classList
                            [ ( "elmsh-hl", highlight == Just Normal )
                            , ( "elmsh-add", highlight == Just Add )
                            , ( "elmsh-del", highlight == Just Delete )
                            ]
                        ]
                        (List.map elementView fragments)
                    ]
            )
        |> List.concat
        |> code [ class "elmsh" ]


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
