module SyntaxHighlight.Custom exposing
    ( fromParser
    , Fragment
    , fragment, newline, setFragmentStyle, setFragmentClasses
    , Style
    , styleDefault, styleComment, style1, style2, style3, style4, style5, style6, style7
    )

{-|

@docs fromParser

@docs Fragment

@docs fragment, newline, setFragmentStyle, setFragmentClasses

@docs Style

@docs styleDefault, styleComment, style1, style2, style3, style4, style5, style6, style7

-}

import Parser exposing (Parser)
import SyntaxHighlight exposing (Highlight(..))
import SyntaxHighlight.Language.Type exposing (HCode(..))
import SyntaxHighlight.Line as Line exposing (Line)
import SyntaxHighlight.Style as Style


{-| Use a parser from `elm/parser` to define your own syntax. Your parser must
produce a list of `Fragment` values out of the code string.
-}
fromParser : Parser (List Fragment) -> String -> Result (List Parser.DeadEnd) SyntaxHighlight.HCode
fromParser parser code =
    Parser.run parser code
        |> Result.map
            (\fragments ->
                fragments
                    |> List.concatMap lineBreaksAsFragments
                    |> fragmentsIntoLines emptyLine []
                    |> SyntaxHighlight.toHCode
            )



-- FRAGMENT


{-| One single styled portion of a line of parsed code. Holds information about
the text being styled, the style and additional class to be applied.
-}
type Fragment
    = Fragment Line.Fragment
    | Newline (Maybe Highlight)


{-| Constructs a `Fragment` value out of a part of the code you're parsing.
Typically should represent one syntactic unit in the code. You can add styling
details to this `Fragment` using `setFragmentStyle` and `setFragmentClasses`.
-}
fragment : String -> Fragment
fragment text =
    Fragment
        { text = text
        , requiredStyle = Style.Default
        , additionalClass = ""
        }


{-| A special kind of `Fragment`: a way to insert an explicit break into a new
line when parsing your code. You can optionally pass it a `Highlight` value to
change how the following line is rendered.
-}
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


{-| -}
type Style
    = Style Style.Required


{-| -}
styleDefault : Style
styleDefault =
    Style Style.Default


{-| -}
styleComment : Style
styleComment =
    Style Style.Comment


{-| -}
style1 : Style
style1 =
    Style Style.Style1


{-| -}
style2 : Style
style2 =
    Style Style.Style2


{-| -}
style3 : Style
style3 =
    Style Style.Style3


{-| -}
style4 : Style
style4 =
    Style Style.Style4


{-| -}
style5 : Style
style5 =
    Style Style.Style5


{-| -}
style6 : Style
style6 =
    Style Style.Style6


{-| -}
style7 : Style
style7 =
    Style Style.Style7



-- INTERNAL


{-| Splits a fragment on every `'\n'` character, and puts `LineBreak Nothing`
values in between.
-}
lineBreaksAsFragments : Fragment -> List Fragment
lineBreaksAsFragments f =
    case f of
        Fragment lf ->
            String.split "\n" lf.text
                |> List.map (\text -> Fragment { lf | text = text })
                |> List.intersperse (Newline Nothing)
                |> List.filter (isEmptyFragment >> not)

        Newline _ ->
            [ f ]


{-| Splits fragments into lines where `Newline` values are found. It's a
recursive algorithm that takes an initial empty `Line`, a list of accumulated
lines, and finally the fragments we're going to place into lines.
-}
fragmentsIntoLines : Line -> List Line -> List Fragment -> List Line
fragmentsIntoLines accLine accLines fragments =
    case fragments of
        [] ->
            (reverseLineFragments accLine :: accLines)
                |> List.reverse

        (Newline hl) :: restFragments ->
            if accLine == emptyLine && accLines == [] then
                -- This is the first fragment, and it's a `Newline`. We skip
                -- creating the new `Line` and instead update the current
                -- (first) line's highlight value.
                fragmentsIntoLines
                    { accLine | highlight = hl |> Maybe.map toInternalHighlight }
                    accLines
                    restFragments

            else
                fragmentsIntoLines
                    { emptyLine | highlight = hl |> Maybe.map toInternalHighlight }
                    (reverseLineFragments accLine :: accLines)
                    restFragments

        (Fragment lf) :: restFragments ->
            fragmentsIntoLines
                { accLine | fragments = lf :: accLine.fragments }
                accLines
                restFragments


isEmptyFragment : Fragment -> Bool
isEmptyFragment f =
    case f of
        Fragment lf ->
            lf.text == ""

        Newline _ ->
            False


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
