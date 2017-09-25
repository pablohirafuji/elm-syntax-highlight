module Main exposing (..)

import Time exposing (Time)
import Html exposing (Html, div, text, p, textarea, pre, code, option, select, label, ul, li, input, button)
import Html.Attributes exposing (defaultValue, id, class, value, spellcheck, selected, style, type_, placeholder, checked, classList)
import Dict exposing (Dict)
import Html.Lazy
import Html.Events exposing (onClick, onInput, onCheck)
import Json.Decode as Json
import SyntaxHighlight as SH
import SyntaxHighlight.Theme as Theme
import AnimationFrame


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> AnimationFrame.times Frame
        }



-- Model


type alias Model =
    { scroll : Scroll
    , currentLanguage : String
    , languagesModel : Dict String LanguageModel
    , showLineCount : Bool
    , lineCountStart : Int
    , lineCount : Maybe Int
    , theme : String
    , customTheme : String
    , highlight : HighlightModel
    }


initModel : Model
initModel =
    { scroll = Scroll 0 0
    , currentLanguage = "Elm"
    , languagesModel = initLanguagesModel
    , showLineCount = True
    , lineCountStart = 1
    , lineCount = Just 1
    , theme = "Monokai"
    , customTheme = Theme.monokai
    , highlight = HighlightModel (Just SH.Add) 1 3
    }


type alias Scroll =
    { top : Int
    , left : Int
    }


type alias LanguageModel =
    { code : String
    , scroll : Scroll
    , highlight : HighlightModel
    }


initLanguagesModel : Dict String LanguageModel
initLanguagesModel =
    Dict.fromList
        [ ( "Elm", initLanguageModel elmExample )
        , ( "Xml", initLanguageModel xmlExample )
        , ( "Javascript", initLanguageModel javascriptExample )
        , ( "Css", initLanguageModel cssExample )
        ]


initLanguageModel : String -> LanguageModel
initLanguageModel codeStr =
    { code = codeStr
    , scroll = Scroll 0 0
    , highlight = initHighlightModel
    }


type alias HighlightModel =
    { mode : Maybe SH.Highlight
    , start : Int
    , end : Int
    }


initHighlightModel : HighlightModel
initHighlightModel =
    { mode = Nothing
    , start = 0
    , end = 0
    }


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


cssExample : String
cssExample =
    """stock::before {
  display: block;
  content: "To scale, the lengths of materials in stock are:";
}
stock > * {
  display: block;
  width: attr(length em); /* default 0 */
  height: 1em;
  border: solid thin;
  margin: 0.5em;
}
.wood {
  background: orange url(wood.png);
}
.metal {
  background: #c0c0c0 url(metal.png);
}
"""



-- Update


type Msg
    = NoOp
    | SetText String String
    | OnScroll Scroll
    | Frame Time
    | SetLanguage String
    | ShowLineCount Bool
    | SetLineCountStart Int
    | SetColorScheme String
    | SetCustomColorScheme String
    | SetHighlightMode (Maybe SH.Highlight)
    | SetHighlightStart Int
    | SetHighlightEnd Int
    | ApplyHighlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ highlight } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetText lang codeStr ->
            getLangModel lang model
                |> (\m -> { m | code = codeStr })
                |> updateLangModel lang model
                |> flip (,) Cmd.none

        OnScroll scroll ->
            ( { model | scroll = scroll }
            , Cmd.none
            )

        Frame _ ->
            getLangModel model.currentLanguage model
                |> (\m -> { m | scroll = model.scroll })
                |> updateLangModel model.currentLanguage model
                |> flip (,) Cmd.none

        SetLanguage lang ->
            getLangModel lang model
                |> (\m -> { m | scroll = Scroll 0 0 })
                |> updateLangModel lang model
                |> (\m ->
                        { m
                            | scroll = Scroll 0 0
                            , currentLanguage = lang
                        }
                   )
                |> flip (,) Cmd.none

        ShowLineCount bool ->
            ( { model
                | showLineCount = bool
                , lineCount =
                    if bool then
                        Just model.lineCountStart
                    else
                        Nothing
              }
            , Cmd.none
            )

        SetLineCountStart start ->
            ( { model
                | lineCountStart = start
                , lineCount = Just start
              }
            , Cmd.none
            )

        SetColorScheme cs ->
            ( { model | theme = cs }
            , Cmd.none
            )

        SetCustomColorScheme ccs ->
            ( { model | customTheme = ccs }
            , Cmd.none
            )

        SetHighlightMode mode ->
            ( { model | highlight = { highlight | mode = mode } }
            , Cmd.none
            )

        SetHighlightStart int ->
            ( { model | highlight = { highlight | start = int } }
            , Cmd.none
            )

        SetHighlightEnd int ->
            ( { model | highlight = { highlight | end = int } }
            , Cmd.none
            )

        ApplyHighlight ->
            getLangModel model.currentLanguage model
                |> (\m -> { m | highlight = model.highlight })
                |> updateLangModel model.currentLanguage model
                |> flip (,) Cmd.none


getLangModel : String -> Model -> LanguageModel
getLangModel lang model =
    Dict.get lang model.languagesModel
        |> Maybe.withDefault (initLanguageModel elmExample)


updateLangModel : String -> Model -> LanguageModel -> Model
updateLangModel lang model langModel =
    Dict.insert lang langModel model.languagesModel
        |> \n -> { model | languagesModel = n }



-- View


view : Model -> Html Msg
view model =
    div []
        [ Html.node "style" [] [ text (textareaStyle model) ]
        , Html.Lazy.lazy2 syntaxTheme model.theme model.customTheme
        , viewLanguage "Elm" toHtmlElm model
        , viewLanguage "Javascript" toHtmlJavascript model
        , viewLanguage "Xml" toHtmlXml model
        , viewLanguage "Css" toHtmlCss model
        , viewOptions model
        ]


textareaStyle : Model -> String
textareaStyle { theme } =
    let
        style a b =
            String.join "\n"
                [ ".textarea {caret-color: " ++ a ++ ";}"
                , ".textarea::selection { background-color: " ++ b ++ "; }"
                ]
    in
        if List.member theme [ "Monokai", "One Dark", "Custom" ] then
            style "#f8f8f2" "rgba(255,255,255,0.2)"
        else
            style "#24292e" "rgba(0,0,0,0.2)"


syntaxTheme : String -> String -> Html msg
syntaxTheme currentTheme customTheme =
    Dict.fromList Theme.all
        |> Dict.get currentTheme
        |> Maybe.withDefault customTheme
        |> text
        |> List.singleton
        |> Html.node "style" []


viewLanguage : String -> (Maybe Int -> String -> HighlightModel -> Html Msg) -> Model -> Html Msg
viewLanguage thisLang parser ({ currentLanguage, lineCount } as model) =
    if thisLang /= currentLanguage then
        div [] []
    else
        let
            langModel =
                getLangModel thisLang model
        in
            div
                [ classList
                    [ ( "container", True )
                    , ( "elmsh", True )
                    ]
                ]
                [ div
                    [ class "view-container"
                    , style
                        [ ( "transform"
                          , "translate("
                                ++ toString -langModel.scroll.left
                                ++ "px, "
                                ++ toString -langModel.scroll.top
                                ++ "px)"
                          )
                        , ( "will-change", "transform" )
                        ]
                    ]
                    [ Html.Lazy.lazy3 parser
                        lineCount
                        langModel.code
                        langModel.highlight
                    ]
                , viewTextarea thisLang langModel.code model
                ]


viewTextarea : String -> String -> Model -> Html Msg
viewTextarea thisLang codeStr { showLineCount } =
    textarea
        [ defaultValue codeStr
        , classList
            [ ( "textarea", True )
            , ( "textarea-lc", showLineCount )
            ]
        , onInput (SetText thisLang)
        , spellcheck False
        , Html.Events.on "scroll"
            (Json.map2 Scroll
                (Json.at [ "target", "scrollTop" ] Json.int)
                (Json.at [ "target", "scrollLeft" ] Json.int)
                |> Json.map OnScroll
            )
        ]
        []



-- Helpers function for Html.Lazy.lazy


toHtmlElm : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlElm =
    toHtml SH.elm


toHtmlXml : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlXml =
    toHtml SH.xml


toHtmlJavascript : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlJavascript =
    toHtml SH.javascript


toHtmlCss : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlCss =
    toHtml SH.css


toHtml : (String -> Result x SH.HCode) -> Maybe Int -> String -> HighlightModel -> Html Msg
toHtml parser maybeStart str hlModel =
    parser str
        |> Result.map (SH.highlightLines hlModel.mode hlModel.start hlModel.end)
        |> Result.map (SH.toBlockHtml maybeStart)
        |> Result.mapError (\x -> text (toString x))
        |> (\result ->
                case result of
                    Result.Ok a ->
                        a

                    Result.Err x ->
                        x
           )



-- Options


viewSelectOptions : String -> List String -> List (Html Msg)
viewSelectOptions current =
    List.map
        (\name_ ->
            option
                [ selected (current == name_), value name_ ]
                [ text name_ ]
        )


viewOptions : Model -> Html Msg
viewOptions ({ currentLanguage, showLineCount, lineCountStart, theme } as model) =
    ul []
        [ li []
            [ label []
                [ input
                    [ type_ "checkbox"
                    , checked showLineCount
                    , onCheck ShowLineCount
                    ]
                    []
                , text "Show Line Count"
                ]
            , if showLineCount then
                numberInput " - Start: " lineCountStart SetLineCountStart
              else
                text ""
            ]
        , li []
            [ label []
                [ text "Language: "
                , select
                    [ Json.at [ "target", "value" ] Json.string
                        |> Json.map SetLanguage
                        |> Html.Events.on "change"
                    ]
                  <|
                    viewSelectOptions
                        model.currentLanguage
                        (Dict.keys model.languagesModel)
                ]
            ]
        , li []
            [ label []
                [ text "Color Scheme: "
                , select
                    [ Html.Events.on "change"
                        (Json.map SetColorScheme (Json.at [ "target", "value" ] Json.string))
                    ]
                  <|
                    viewSelectOptions
                        model.theme
                        ((List.map Tuple.first Theme.all)
                            ++ [ "Custom" ]
                        )
                ]
            ]
        , if theme == "Custom" then
            customTheme model
          else
            text ""
        , li []
            [ text "Highlight Lines"
            , viewHighlightOptions model.highlight
            ]
        ]


customTheme : Model -> Html Msg
customTheme model =
    textarea
        [ defaultValue model.customTheme
        , onInput SetCustomColorScheme
        , spellcheck False
        , style [ ( "width", "100%" ) ]
        , Html.Attributes.rows 10
        ]
        []


viewHighlightOptions : HighlightModel -> Html Msg
viewHighlightOptions { mode, start, end } =
    ul []
        [ li []
            [ label []
                [ text "Type: "
                , select
                    [ Json.at [ "target", "value" ] Json.string
                        |> Json.map toHighlightMode
                        |> Json.map SetHighlightMode
                        |> Html.Events.on "change"
                    ]
                    [ option [ selected (mode == Nothing) ] [ text "No highlight" ]
                    , option [ selected (mode == Just SH.Highlight) ] [ text "Highlight" ]
                    , option [ selected (mode == Just SH.Add) ] [ text "Addition" ]
                    , option [ selected (mode == Just SH.Del) ] [ text "Deletion" ]
                    ]
                ]
            ]
        , li [] [ numberInput "Start: " start SetHighlightStart ]
        , li [] [ numberInput "End: " end SetHighlightEnd ]
        , li [] [ button [ onClick ApplyHighlight ] [ text "Highlight" ] ]
        ]


toHighlightMode : String -> Maybe SH.Highlight
toHighlightMode str =
    case str of
        "Highlight" ->
            Just SH.Highlight

        "Addition" ->
            Just SH.Add

        "Deletion" ->
            Just SH.Del

        _ ->
            Nothing


numberInput : String -> Int -> (Int -> Msg) -> Html Msg
numberInput labelStr defaultVal msg =
    label []
        [ text labelStr
        , input
            [ type_ "number"
            , Html.Attributes.min "-999"
            , Html.Attributes.max "999"
            , onInput (String.toInt >> Result.withDefault 0 >> msg)
            , defaultValue (toString defaultVal)
            ]
            []
        ]
