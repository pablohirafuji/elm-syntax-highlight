module SyntaxHighlight.Custom exposing (Fragment, Line, Style, fragment, fromParser, line, setFragmentClasses, setFragmentStyle, setLineHighlight, style1, style2, style3, style4, style5, style6, style7, styleComment, styleDefault)

{-| -}

import Parser exposing (Parser)
import SyntaxHighlight exposing (Highlight(..))
import SyntaxHighlight.Language.Type exposing (HCode(..))
import SyntaxHighlight.Line as Line
import SyntaxHighlight.Style as Style


{-| Use a parser from `elm/parser` to define your own syntax. Your parser must
produce a list of `Line` values out of the code string.
-}
fromParser : Parser (List Line) -> String -> Result (List Parser.DeadEnd) HCode
fromParser parser code =
    Parser.run parser code
        |> Result.map
            (\lines ->
                HCode (lines |> List.map (\(Line line_) -> line_))
            )



-- LINE


{-| A line of parsed code. Holds information about its `Fragment`s and if is
highlighted in any way.
-}
type Line
    = Line Line.Line


{-| Constructs one line of a `customSyntax` parser, out of a list of `Fragment`
values, which you can in turn construct using the `fragment` function.
-}
line : List Fragment -> Line
line fragments =
    Line
        { fragments = fragments |> List.map (\(Fragment fragment_) -> fragment_)
        , highlight = Nothing
        }


{-| You can make a `Line` in a custom syntax look highlighted, or as a diff
addition/deletion, using this function.
-}
setLineHighlight : Maybe Highlight -> Line -> Line
setLineHighlight highlight_ (Line line_) =
    let
        convertedHighlight =
            highlight_
                |> Maybe.map
                    (\hl ->
                        case hl of
                            Highlight ->
                                Line.Normal

                            Add ->
                                Line.Add

                            Del ->
                                Line.Del
                    )
    in
    Line { line_ | highlight = convertedHighlight }



-- FRAGMENT


{-| One single styled portion of a line of parsed code. Holds information about
the text being styled, the style and additional class to be applied.
-}
type Fragment
    = Fragment Line.Fragment


{-| Constructs a `Fragment` value out of a `String`, which is one part of a
`Line` for a custom syntax. Check the `customSyntax` function for more details.
-}
fragment : String -> Fragment
fragment text =
    Fragment
        { text = text
        , requiredStyle = Style.Default
        , additionalClass = ""
        }


{-| Sets a specific style to a `Fragment`, which gives it a different color
depending on the theme used.
-}
setFragmentStyle : Style -> Fragment -> Fragment
setFragmentStyle (Style style) (Fragment fragment_) =
    Fragment { fragment_ | requiredStyle = style }


{-| You can optionally use this function to give a `Fragment` one or more custom
CSS classes (separated by spaces), if you want more control over how you style
your custom syntax.
-}
setFragmentClasses : String -> Fragment -> Fragment
setFragmentClasses classes (Fragment fragment_) =
    Fragment { fragment_ | additionalClass = classes }



-- STYLE


type Style
    = Style Style.Required


styleDefault : Style
styleDefault =
    Style Style.Default


styleComment : Style
styleComment =
    Style Style.Comment


style1 : Style
style1 =
    Style Style.Style1


style2 : Style
style2 =
    Style Style.Style2


style3 : Style
style3 =
    Style Style.Style3


style4 : Style
style4 =
    Style Style.Style4


style5 : Style
style5 =
    Style Style.Style5


style6 : Style
style6 =
    Style Style.Style6


style7 : Style
style7 =
    Style Style.Style7
