module SyntaxHighlight.Custom exposing (Fragment, Style, fragment, fromParser, newline, setFragmentClasses, setFragmentStyle, style1, style2, style3, style4, style5, style6, style7, styleComment, styleDefault)

{-| -}

import Parser exposing (Parser)
import SyntaxHighlight exposing (Highlight(..))
import SyntaxHighlight.Language.Type exposing (HCode(..))
import SyntaxHighlight.Line as Line exposing (Line)
import SyntaxHighlight.Style as Style


{-| Use a parser from `elm/parser` to define your own syntax. Your parser must
produce a list of `Line` values out of the code string.
-}
fromParser : Parser (List Fragment) -> String -> Result (List Parser.DeadEnd) HCode
fromParser parser code =
    Parser.run parser code
        |> Result.map
            (\fragments ->
                fragments
                    |> List.concatMap lineBreaksAsFragments
                    |> fragmentsIntoLines emptyLine []
                    |> HCode
            )



-- FRAGMENT


{-| One single styled portion of a line of parsed code. Holds information about
the text being styled, the style and additional class to be applied.
-}
type Fragment
    = Fragment Line.Fragment
    | Newline (Maybe Highlight)


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


newline : Maybe Highlight -> Fragment
newline hl =
    Newline hl


{-| Sets a specific style to a `Fragment`, which gives it a different color
depending on the theme used.
-}
setFragmentStyle : Style -> Fragment -> Fragment
setFragmentStyle (Style style) f =
    case f of
        Fragment lf ->
            Fragment { lf | requiredStyle = style }

        Newline _ ->
            f


{-| You can optionally use this function to give a `Fragment` one or more custom
CSS classes (separated by spaces), if you want more control over how you style
your custom syntax.
-}
setFragmentClasses : String -> Fragment -> Fragment
setFragmentClasses classes f =
    case f of
        Fragment lf ->
            Fragment { lf | additionalClass = classes }

        Newline _ ->
            f



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



-- INTERNAL


lineBreaksAsFragments : Fragment -> List Fragment
lineBreaksAsFragments f =
    case f of
        Fragment lf ->
            String.split "\n" lf.text
                |> List.map (\text -> Fragment { lf | text = text })
                |> List.intersperse (Newline Nothing)

        Newline _ ->
            [ f ]


fragmentsIntoLines : Line -> List Line -> List Fragment -> List Line
fragmentsIntoLines accLine accLines fragments =
    case fragments of
        [] ->
            (reverseLineFragments accLine :: accLines)
                |> List.reverse

        (Newline hl) :: restFragments ->
            fragmentsIntoLines
                { emptyLine | highlight = hl |> Maybe.map toInternalHighlight }
                (reverseLineFragments accLine :: accLines)
                restFragments

        (Fragment lf) :: restFragments ->
            fragmentsIntoLines
                { accLine | fragments = lf :: accLine.fragments }
                accLines
                restFragments


emptyLine : Line
emptyLine =
    { fragments = [], highlight = Nothing }


reverseLineFragments : Line -> Line
reverseLineFragments line =
    { line | fragments = List.reverse line.fragments }


toInternalHighlight : Highlight -> Line.Highlight
toInternalHighlight hl =
    case hl of
        Highlight ->
            Line.Normal

        Add ->
            Line.Add

        Del ->
            Line.Del
