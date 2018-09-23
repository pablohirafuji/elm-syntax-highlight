port module Main exposing (Msg, ThemeModel, init, main, requiredStyles, themesList, update)

import SyntaxHighlight as SH
import SyntaxHighlight.Theme as Theme


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- TYPES


type alias Msg =
    ()


type alias ThemeModel =
    { name : String
    , content : String
    }


init : ( (), Cmd Msg )
init =
    ( ()
    , themesList
        (List.map
            (\( name, content ) ->
                ThemeModel
                    name
                    (SH.css (String.trim content)
                        |> Result.map (SH.toStaticBlockHtml (Just 1))
                        |> Result.withDefault (String.trim content)
                    )
            )
            (requiredStyles :: Theme.all)
        )
    )


requiredStyles : ( String, String )
requiredStyles =
    ( "Required Styles", """pre.elmsh {
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
}""" )


update : Msg -> () -> ( (), Cmd Msg )
update msg model =
    ( model, Cmd.none )


port themesList : List ThemeModel -> Cmd msg
