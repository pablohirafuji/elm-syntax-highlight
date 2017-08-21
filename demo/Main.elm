module Main exposing (..)

import Time exposing (Time)
import Html exposing (Html, div, text, p, textarea, pre, code, option, select, label, ul, li, input, button)
import Html.Attributes exposing (defaultValue, id, class, value, spellcheck, selected, style, type_, placeholder, checked, classList)
import Html.Lazy
import Html.Events exposing (onClick, onInput, onCheck)
import Json.Decode as Json
import SyntaxHighlight as SH
import SyntaxHighlight.Line as SH exposing (Line, Highlight(..))
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
    , showLineCount : Bool
    , lineCountStart : Int
    , lineCount : Maybe Int
    , colorScheme : String
    , highlight : HighlightModel
    }


initModel : Model
initModel =
    { scroll = Scroll 0 0
    , selection = Nothing
    , language = Elm
    , elm = initLanguageModel elmExample
    , javascript = initLanguageModel javascriptExample
    , xml = initLanguageModel xmlExample
    , showLineCount = True
    , lineCountStart = 1
    , lineCount = Just 1
    , colorScheme = "monokai"
    , highlight = HighlightModel (Just Add) 1 3
    }


type alias Scroll =
    { top : Int
    , left : Int
    }


type alias Selection =
    { start : Int
    , end : Int
    }


type Language
    = Elm
    | Javascript
    | Xml


type alias LanguageModel =
    { code : String
    , scroll : Scroll
    , highlight : HighlightModel
    }


initLanguageModel : String -> LanguageModel
initLanguageModel codeStr =
    { code = codeStr
    , scroll = Scroll 0 0
    , highlight = initHighlightModel
    }


type alias HighlightModel =
    { mode : Maybe Highlight
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


type Msg
    = NoOp
    | SetText Language String
    | OnScroll Scroll
    | Frame Time
    | SetLanguage Language
    | OnSelect Selection
    | ShowLineCount Bool
    | SetLineCountStart Int
    | SetColorScheme String
    | SetHighlightMode (Maybe Highlight)
    | SetHighlightStart Int
    | SetHighlightEnd Int
    | ApplyHighlight


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ elm, xml, javascript, highlight } as model) =
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
            getLangModel model.language model
                |> (\m -> { m | scroll = model.scroll })
                |> updateLangModel model.language model
                |> flip (,) Cmd.none

        SetLanguage lang ->
            getLangModel lang model
                |> (\m -> { m | scroll = Scroll 0 0 })
                |> updateLangModel lang model
                |> (\m ->
                        { m
                            | scroll = Scroll 0 0
                            , language = lang
                        }
                   )
                |> flip (,) Cmd.none

        OnSelect selection ->
            ( model, Cmd.none )

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
            ( { model | colorScheme = cs }
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
            getLangModel model.language model
                |> (\m -> { m | highlight = model.highlight })
                |> updateLangModel model.language model
                |> flip (,) Cmd.none


getLangModel : Language -> Model -> LanguageModel
getLangModel lang model =
    case lang of
        Elm ->
            model.elm

        Xml ->
            model.xml

        Javascript ->
            model.javascript


updateLangModel : Language -> Model -> LanguageModel -> Model
updateLangModel lang model langModel =
    case lang of
        Elm ->
            { model | elm = langModel }

        Xml ->
            { model | xml = langModel }

        Javascript ->
            { model | javascript = langModel }


view : Model -> Html Msg
view ({ language } as model) =
    div []
        [ Html.node "style" [] [ text (textareaStyle model) ]
        , syntaxTheme model
        , viewLanguage Elm model
        , viewLanguage Javascript model
        , viewLanguage Xml model
        , viewOptions model
        ]


textareaStyle : Model -> String
textareaStyle { colorScheme } =
    if colorScheme == "monokai" then
        """.textarea {caret-color: #f8f8f2;}
.textarea::selection {
    background-color: rgba(255,255,255,0.2);
}"""
    else
        """.textarea {caret-color: #24292e;}
.textarea::selection {
    background-color: rgba(0,0,0,0.2);
}"""


syntaxTheme : Model -> Html msg
syntaxTheme { showLineCount, colorScheme } =
    if colorScheme == "monokai" then
        SH.useTheme SH.monokai
    else
        SH.useTheme SH.github


viewLanguage : Language -> Model -> Html Msg
viewLanguage thisLang ({ language, lineCount } as model) =
    if thisLang /= language then
        div [] []
    else
        let
            ( langModel, parser ) =
                getLangModelParser thisLang model
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
                          , "translate(" ++ toString -langModel.scroll.left ++ "px, " ++ toString -langModel.scroll.top ++ "px)"
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


viewTextarea : Language -> String -> Model -> Html Msg
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
        , Html.Events.on "select"
            (Json.map2 Selection
                (Json.at [ "target", "selectionStart" ] Json.int)
                (Json.at [ "target", "selectionEnd" ] Json.int)
                |> Json.map OnSelect
            )
        ]
        []


getLangModelParser : Language -> Model -> ( LanguageModel, Maybe Int -> String -> HighlightModel -> Html Msg )
getLangModelParser lang model =
    case lang of
        Elm ->
            ( model.elm, toHtmlElm )

        Xml ->
            ( model.xml, toHtmlXml )

        Javascript ->
            ( model.javascript, toHtmlJavascript )


viewOptions : Model -> Html Msg
viewOptions ({ language, showLineCount, lineCountStart, colorScheme } as model) =
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
                        |> Json.map toLanguageType
                        |> Json.map SetLanguage
                        |> Html.Events.on "change"
                    ]
                    [ option [ selected (language == Elm) ] [ text "Elm" ]
                    , option [ selected (language == Xml) ] [ text "Xml" ]
                    , option [ selected (language == Javascript) ] [ text "Javascript" ]
                    ]
                ]
            ]
        , li []
            [ label []
                [ text "Color Scheme: "
                , select
                    [ Html.Events.on "change"
                        (Json.map SetColorScheme (Json.at [ "target", "value" ] Json.string))
                    ]
                    [ option [ selected (colorScheme == "monokai"), value "monokai" ] [ text "Monokai" ]
                    , option [ selected (colorScheme == "github"), value "github" ] [ text "GitHub" ]
                    ]
                ]
            ]
        , li []
            [ text "Highlight Lines"
            , viewHighlightOptions model.highlight
            ]
        ]


toLanguageType : String -> Language
toLanguageType str =
    case str of
        "Elm" ->
            Elm

        "Xml" ->
            Xml

        _ ->
            Javascript


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
                    , option [ selected (mode == Just Normal) ] [ text "Highlight" ]
                    , option [ selected (mode == Just Add) ] [ text "Addition" ]
                    , option [ selected (mode == Just Delete) ] [ text "Deletion" ]
                    ]
                ]
            ]
        , li [] [ numberInput "Start: " start SetHighlightStart ]
        , li [] [ numberInput "End: " end SetHighlightEnd ]
        , li [] [ button [ onClick ApplyHighlight ] [ text "Highlight" ] ]
        ]


toHighlightMode : String -> Maybe Highlight
toHighlightMode str =
    case str of
        "Highlight" ->
            Just Normal

        "Addition" ->
            Just Add

        "Deletion" ->
            Just Delete

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


toHtml : (String -> Result x (List Line)) -> Maybe Int -> String -> HighlightModel -> Html Msg
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
