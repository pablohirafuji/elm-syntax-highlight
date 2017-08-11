module SyntaxHighlight.View exposing (..)

import Html exposing (Html, text, span, br, code)
import Html.Attributes exposing (classList)
import Parser exposing (Error)
import SyntaxHighlight.Style exposing (..)


toHtml : Result Error (List ( Style, String )) -> Html msg
toHtml result =
    result
        |> Result.map (List.map toElement >> code [])
        |> Result.mapError (\x -> code [] [ text (toString x) ])
        |> extractResult


toElement : ( Style, String ) -> Html msg
toElement ( { color, isItalic, isBold }, str ) =
    if color == Default && not isItalic && not isBold then
        text str
    else
        span
            [ classList
                [ ( "elmsh" ++ toString color, color /= Default )
                , ( "elmshItalic", isItalic )
                , ( "elmshBold", isBold )
                ]
            ]
            [ text str ]


extractResult : Result a a -> a
extractResult result =
    case result of
        Result.Ok a ->
            a

        Result.Err a ->
            a
