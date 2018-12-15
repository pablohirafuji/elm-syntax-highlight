module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Dict exposing (Dict)
import Html exposing (Html, a, button, code, div, h1, input, label, li, node, option, p, pre, select, small, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, href, id, placeholder, selected, spellcheck, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Html.Lazy
import Json.Decode as Json
import Parser
import SyntaxHighlight as SH exposing (Theme)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> onAnimationFrame (\_ -> Frame)
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
    , customTheme = rawMonokai
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
        , ( "Python", initLanguageModel pythonExample )
        , ( "Sql", initLanguageModel sqlExample )
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


pythonExample : String
pythonExample =
    """ice_cream = 'chocolate'
if ice_cream == 'chocolate':
    print('Yay, I love chocolate ice cream!')
else:
    print('Awwww, but chocolate is my favorite...');

# Multiply two numbers
def multiply(a, b):
    return a * b

class Animal:
    def __init__(self):
        pass

class Dog(Animal):
    kind = 'canine'

    def __init__(self, name):
        self.name = name

d = Dog('Fido')
"""


sqlExample : String
sqlExample =
    """/*
 * multi-line comment
 */
SELECT a, b, m.*
  from w.t,
       w.t2 AS m
 WHERE a = avg(b)
    or b != 0x1aB5
   and c is True
   AND m.key is in ['key\\'1','key2'];
"""



-- Update


type Msg
    = NoOp
    | SetText String String
    | OnScroll Scroll
    | Frame
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
                |> (\a -> ( a, Cmd.none ))

        OnScroll scroll ->
            ( { model | scroll = scroll }
            , Cmd.none
            )

        Frame ->
            getLangModel model.currentLanguage model
                |> (\m -> { m | scroll = model.scroll })
                |> updateLangModel model.currentLanguage model
                |> (\a -> ( a, Cmd.none ))

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
                |> (\a -> ( a, Cmd.none ))

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
                |> (\a -> ( a, Cmd.none ))


getLangModel : String -> Model -> LanguageModel
getLangModel lang model =
    Dict.get lang model.languagesModel
        |> Maybe.withDefault (initLanguageModel elmExample)


updateLangModel : String -> Model -> LanguageModel -> Model
updateLangModel lang model langModel =
    Dict.insert lang langModel model.languagesModel
        |> (\n -> { model | languagesModel = n })



-- View


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text baseStyles ]
        , h1 []
            [ text "Elm Syntax Highlight "
            , small []
                [ text "v3.1.0" ]
            ]
        , p [ class "subheading" ]
            [ a [ href "http://package.elm-lang.org/packages/pablohirafuji/elm-syntax-highlight/latest" ]
                [ text "Package" ]
            , text "/ "
            , a [ href "https://github.com/pablohirafuji/elm-syntax-highlight" ]
                [ text "GitHub" ]
            , text "/ "
            , a [ href "https://pablohirafuji.github.io/elm-syntax-highlight/themes.html" ]
                [ text "Themes" ]
            , text "/ "
            , a [ href "https://github.com/pablohirafuji/elm-syntax-highlight/blob/master/demo/Main.elm" ]
                [ text "Source" ]
            ]
        , Html.Lazy.lazy textareaStyle model.theme
        , Html.Lazy.lazy2 syntaxThemeStyle model.theme model.customTheme
        , viewLanguage "Elm" toHtmlElm model
        , viewLanguage "Javascript" toHtmlJavascript model
        , viewLanguage "Xml" toHtmlXml model
        , viewLanguage "Css" toHtmlCss model
        , viewLanguage "Python" toHtmlPython model
        , viewLanguage "Sql" toHtmlSql model
        , viewOptions model
        ]


textareaStyle : String -> Html msg
textareaStyle theme =
    let
        style a b =
            Html.node "style"
                []
                [ text
                    (String.join "\n"
                        [ ".textarea {caret-color: " ++ a ++ ";}"
                        , ".textarea::selection { background-color: " ++ b ++ "; }"
                        ]
                    )
                ]
    in
    if List.member theme [ "Monokai", "One Dark", "Custom" ] then
        style "#f8f8f2" "rgba(255,255,255,0.2)"

    else
        style "#24292e" "rgba(0,0,0,0.2)"


syntaxThemeStyle : String -> String -> Html msg
syntaxThemeStyle selectedTheme customTheme =
    case selectedTheme of
        "Monokai" ->
            SH.useTheme SH.monokai

        "GitHub" ->
            SH.useTheme SH.gitHub

        "One Dark" ->
            SH.useTheme SH.oneDark

        _ ->
            Html.node "style" [] [ text customTheme ]


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
                , style "transform"
                    ("translate("
                        ++ String.fromInt -langModel.scroll.left
                        ++ "px, "
                        ++ String.fromInt -langModel.scroll.top
                        ++ "px)"
                    )
                , style "will-change" "transform"
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
        [ value codeStr
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


toHtmlPython : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlPython =
    toHtml SH.python


toHtmlSql : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlSql =
    toHtml SH.sql


toHtml : (String -> Result (List Parser.DeadEnd) SH.HCode) -> Maybe Int -> String -> HighlightModel -> Html Msg
toHtml parser maybeStart str hlModel =
    parser str
        |> Result.map (SH.highlightLines hlModel.mode hlModel.start hlModel.end)
        |> Result.map (SH.toBlockHtml maybeStart)
        |> Result.mapError Parser.deadEndsToString
        |> (\result ->
                case result of
                    Result.Ok a ->
                        a

                    Result.Err x ->
                        text x
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
                    (viewSelectOptions
                        model.currentLanguage
                        (Dict.keys model.languagesModel)
                    )
                ]
            ]
        , li []
            [ label []
                [ text "Color Scheme: "
                , select
                    [ Html.Events.on "change"
                        (Json.map SetColorScheme (Json.at [ "target", "value" ] Json.string))
                    ]
                    (viewSelectOptions
                        model.theme
                        [ "Monokai"
                        , "GitHub"
                        , "One Dark"
                        , "Custom"
                        ]
                    )
                ]
            ]
        , if theme == "Custom" then
            textarea
                [ value model.customTheme
                , onInput SetCustomColorScheme
                , spellcheck False
                , style "width" "100%"
                , Html.Attributes.rows 10
                ]
                []

          else
            text ""
        , li []
            [ text "Highlight Lines"
            , viewHighlightOptions model.highlight
            ]
        ]


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
            , onInput (String.toInt >> Maybe.withDefault 0 >> msg)
            , value (String.fromInt defaultVal)
            ]
            []
        ]


rawMonokai : String
rawMonokai =
    ".elmsh {color: #f8f8f2;background: #23241f;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"


baseStyles : String
baseStyles =
    """
body {
    margin: 0 auto 20px auto;
    max-width: 650px;
    line-height: 1.6;
    font-size: 18px;
    color: #444;
    padding: 0 10px;
    text-align: center;
}
h1,h2,h3 {
    line-height: 1.2;
}
h1 {
    padding-bottom: 0;
    margin-bottom: 0;
}
h1 small {
    font-size: 1rem;
    color: #888;
}
.subheading {
    margin-top: 0;
}
ul {
    text-align: left;
}
.container {
    position: relative;
    overflow: hidden;
    padding: 0;
    margin: 0;
    text-align: left;
}
.textarea, .view-container {
    box-sizing: border-box;
    font-size: 1rem;
    line-height: 1.2;
    width: 100%;
    height: 100%;
    height: 250px;
    font-family: monospace;
    letter-spacing: normal;
    word-spacing: normal;
    padding: 0;
    margin: 0;
    border: 0;
    background: transparent;
    white-space: pre;
}
.textarea {
    color: rgba(0,0,0,0);
    resize: none;
    z-index: 2;
    position: relative;
    padding: 10px;
}
.textarea-lc {
    padding-left: 70px;
}
.textarea:focus {
    outline: none;
}
.view-container {
    position: absolute;
    top: 0;
    left: 0;
    pointer-events: none;
    z-index:1;
}

/* Elm Syntax Highlight CSS */
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
}

/* Demo specifics */
pre.elmsh {
    overflow: visible;
}
    """
