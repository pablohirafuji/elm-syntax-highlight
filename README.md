# Elm Syntax Highlighting Code Editor

Fork of [elm-syntax-highlight](https://github.com/pablohirafuji/elm-syntax-highlight) that is tailored for code editing.


## Language support

- Elm
- Javascript
- Xml
- Css
- Python (Thanks [@brandly](https://github.com/brandly)!)
- SQL (Thanks [@oschmid](https://github.com/oschmid)!)
- JSON

And there is a `noLang` generic option for when the language is unknown (Thanks [@Anton-4](https://github.com/Anton-4)!).


## Themes

You can define the theme either by copying and pasting the theme styles into your `.css` file or using the `useTheme` helper.

### Copying and pasting the theme

All themes and required styles can be found [here](https://pablohirafuji.github.io/elm-syntax-highlight/themes.html).

### Using `useTheme` helper

Place the `useTheme` function with your chosen theme anywhere on your view.

```elm
import SyntaxHighlight exposing (useTheme, monokai, elm, toBlockHtml)

view : Model -> Html msg
view model =
    div []
        [ useTheme monokai
        , elm model.elmCode
            |> Result.map (toBlockHtml (Just 1))
            |> Result.withDefault
                (pre [] [ code [] [ text model.elmCode ]])
        ]
```

## Thanks

Thank you Evan for bringing joy to the frontend.