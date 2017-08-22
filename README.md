# Elm Syntax Highlight

Syntax highlighting in Elm. [Demo](https://pablohirafuji.github.io/elm-syntax-highlight/).


## Themes

You can define the theme either by copying and pasting the theme styles into your `.css` file or using the `useTheme` helper.

### Copying and pasting the theme

The theme and required styles can be found [here](https://github.com/pablohirafuji/elm-syntax-highlight/blob/master/src/themes.md).

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
