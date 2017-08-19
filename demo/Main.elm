module Main exposing (..)

import Time exposing (Time)
import Html exposing (Html, div, text, p, textarea, pre, code, option, select)
import Html.Attributes exposing (defaultValue, id, class, value, spellcheck, selected, style)
import Html.Lazy exposing (lazy)
import Html.Events exposing (onInput)
import Json.Decode as Json
import SyntaxHighlight as SH
import SyntaxHighlight.Line exposing (Line, Highlight(..))
import AnimationFrame


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> AnimationFrame.times Frame
        }


type alias Model =
    { scroll : Scroll
    , selection : Maybe Selection
    , language : Language
    , elm : LanguageModel
    , javascript : LanguageModel
    , xml : LanguageModel
    }


initModel : Model
initModel =
    { scroll = Scroll 0 0
    , selection = Nothing
    , language = Elm
    , elm = { code = elmExample, scroll = Scroll 0 0 }
    , javascript = { code = javascriptExample, scroll = Scroll 0 0 }
    , xml = { code = xmlExample, scroll = Scroll 0 0 }
    }


type alias Scroll =
    { top : Int
    , left : Int
    }


type alias Selection =
    { start : Int
    , end : Int
    }


type alias LanguageModel =
    { code : String
    , scroll : Scroll
    }


type Language
    = Elm
    | Javascript
    | Xml


elmExample : String
elmExample =
    """module Main exposing (..)

import Html exposing (Html, text)

-- Main function

main : Html a
main =
    text "Hello, World!"
"""


javascriptExample : String
javascriptExample =
    """var iceCream = 'chocolate';
if (iceCream === 'chocolate') {
  alert(`Yay, I love ${iceCream} ice cream!`);
} else {
  alert('Awwww, but chocolate is my favorite...');
}

class Polygon {
  constructor(height, width) {
    this.name = 'Polygon';
    this.height = height;
    this.width = width;
  }
}

// Multiply two numbers

function multiply(num1,num2) {
  var result = num1 * num2;
  return result;
}

"""


xmlExample : String
xmlExample =
    """<html>
<head>
    <title>Elm Syntax Highlight</title>
</head>
<body id="main">
    <p class="hero">Hello World</p>
</body>
</html>
"""


type Msg
    = NoOp
    | SetText Language String
    | OnScroll Scroll
    | Frame Time
    | SetLanguage String
    | OnSelect Selection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ elm, xml, javascript } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetText lang codeStr ->
            ( case lang of
                Elm ->
                    { model | elm = { elm | code = codeStr } }

                Xml ->
                    { model | xml = { xml | code = codeStr } }

                Javascript ->
                    { model | javascript = { javascript | code = codeStr } }
            , Cmd.none
            )

        OnScroll scroll ->
            ( { model | scroll = scroll }
            , Cmd.none
            )

        Frame _ ->
            ( case model.language of
                Elm ->
                    { model | elm = { elm | scroll = model.scroll } }

                Xml ->
                    { model | xml = { xml | scroll = model.scroll } }

                Javascript ->
                    { model | javascript = { javascript | scroll = model.scroll } }
            , Cmd.none
            )

        SetLanguage lang ->
            let
                ( langType, langScroll ) =
                    case lang of
                        "Elm" ->
                            ( Elm, model.elm.scroll )

                        "Xml" ->
                            ( Xml, model.xml.scroll )

                        _ ->
                            ( Javascript, model.javascript.scroll )
            in
                ( { model
                    | scroll = langScroll
                    , language = langType
                  }
                , Cmd.none
                )

        OnSelect selection ->
            let
                x =
                    Debug.log "x" selection
            in
                ( model, Cmd.none )


view : Model -> Html Msg
view ({ language } as model) =
    div []
        [ select
            [ Html.Events.on "change"
                (Json.map SetLanguage (Json.at [ "target", "value" ] Json.string))
            ]
            [ option [ selected (language == Elm) ] [ text "Elm" ]
            , option [ selected (language == Xml) ] [ text "Xml" ]
            , option [ selected (language == Javascript) ] [ text "Javascript" ]
            ]
        , viewLanguage Elm language model.elm toHtmlElm
        , viewLanguage Javascript language model.javascript toHtmlJavascript
        , viewLanguage Xml language model.xml toHtmlXml
        ]


viewLanguage : Language -> Language -> LanguageModel -> (String -> Html Msg) -> Html Msg
viewLanguage lang curLang langModel parser =
    div
        [ class "container elmsh"
        , style
            [ ( "display"
              , if lang == curLang then
                    "block"
                else
                    "none"
              )
            ]
        ]
        [ pre
            [ class "view-container"
            , id ("render" ++ toString lang)
            , style
                [ ( "transform"
                  , "translate(" ++ toString -langModel.scroll.left ++ "px, " ++ toString -langModel.scroll.top ++ "px)"
                  )
                , ( "will-change"
                  , if lang == curLang then
                        "transform"
                    else
                        "auto"
                  )
                ]
            ]
            [ lazy parser langModel.code
            ]
        , textarea
            [ defaultValue langModel.code
            , class "textarea"
            , onInput (SetText lang)
            , spellcheck False
            , Html.Events.on "scroll"
                (Json.map2 Scroll
                    (Json.at [ "target", "scrollTop" ] Json.int)
                    (Json.at [ "target", "scrollLeft" ] Json.int)
                    |> Json.map OnScroll
                )
            , Html.Events.on "select"
                (Json.map2 Selection
                    (Json.at [ "target", "selectionStart" ] Json.int)
                    (Json.at [ "target", "selectionEnd" ] Json.int)
                    |> Json.map OnSelect
                )
            ]
            []
        ]


toHtml : (String -> Result x (List Line)) -> String -> Html Msg
toHtml parser str =
    parser str
        |> Result.map (SH.highlightLines Normal 0 1)
        |> Result.map (SH.highlightLines Add 1 3)
        |> Result.map (SH.highlightLines Delete 3 5)
        |> Result.map SH.toHtml
        |> Result.mapError (\x -> text (toString x))
        |> (\result ->
                case result of
                    Result.Ok a ->
                        a

                    Result.Err x ->
                        x
           )



-- Helpers function for Html.Lazy.lazy


toHtmlElm : String -> Html Msg
toHtmlElm str =
    toHtml SH.elm str


toHtmlXml : String -> Html Msg
toHtmlXml str =
    toHtml SH.xml str


toHtmlJavascript : String -> Html Msg
toHtmlJavascript str =
    toHtml SH.javascript str
