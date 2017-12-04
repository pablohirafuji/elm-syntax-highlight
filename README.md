# Elm Syntax Highlight

Syntax highlighting in Elm. [Demo](https://pablohirafuji.github.io/elm-syntax-highlight/).


## Themes

You can define the theme either by copying and pasting the theme styles into your `.css` file or using the `useTheme` helper.

### Copying and pasting the theme

The theme and required styles can be found [here](https://pablohirafuji.github.io/elm-syntax-highlight/themes.html).

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


## [Changelog](https://github.com/pablohirafuji/elm-syntax-highlight/releases)

## Thanks

Thank you Evan for bringing joy to the frontend.

Donation: BTC [1HAyTwtxMEkJaHdUYMXVvoXp2PHgwFx2M9](https://pablohirafuji.github.io/elm-qrcode/#bitcoin:1HAyTwtxMEkJaHdUYMXVvoXp2PHgwFx2M9)
