module SyntaxHighlight.Custom exposing (Fragment, Style, fragment, fromParser, lineBreak, setFragmentClasses, setFragmentStyle, style1, style2, style3, style4, style5, style6, style7, styleComment, styleDefault)

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
                HCode (fragmentsIntoLines emptyLine [] fragments)
            )



-- FRAGMENT


{-| One single styled portion of a line of parsed code. Holds information about
the text being styled, the style and additional class to be applied.
-}
type Fragment
    = Fragment Line.Fragment
    | LineBreak (Maybe Highlight)


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


lineBreak : Maybe Highlight -> Fragment
lineBreak highlightM =
    LineBreak highlightM


{-| Sets a specific style to a `Fragment`, which gives it a different color
depending on the theme used.
-}
setFragmentStyle : Style -> Fragment -> Fragment
setFragmentStyle (Style style) fragment_ =
    case fragment_ of
        Fragment f ->
            Fragment { f | requiredStyle = style }

        LineBreak _ ->
            fragment_


{-| You can optionally use this function to give a `Fragment` one or more custom
CSS classes (separated by spaces), if you want more control over how you style
your custom syntax.
-}
setFragmentClasses : String -> Fragment -> Fragment
setFragmentClasses classes fragment_ =
    case fragment_ of
        Fragment f ->
            Fragment { f | additionalClass = classes }

        LineBreak _ ->
            fragment_



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


fragmentsIntoLines : Line -> List Line -> List Fragment -> List Line
fragmentsIntoLines accLine accLines fragments =
    case fragments of
        [] ->
            (reverseLineFragments accLine :: accLines)
                |> List.reverse

        (LineBreak hl) :: restFragments ->
            fragmentsIntoLines
                { emptyLine | highlight = hl |> Maybe.map toInternalHighlight }
                (reverseLineFragments accLine :: accLines)
                restFragments

        (Fragment f) :: restFragments ->
            let
                ( newAccLine, newLines ) =
                    splitLFragmentOnLineBreaks f
                        |> placeLFragmentsOnLines accLine []
            in
            fragmentsIntoLines newAccLine (newLines ++ accLines) restFragments


splitLFragmentOnLineBreaks : Line.Fragment -> List Line.Fragment
splitLFragmentOnLineBreaks f =
    String.split "\n" f.text
        |> List.map (\text -> { f | text = text })


placeLFragmentsOnLines : Line -> List Line -> List Line.Fragment -> ( Line, List Line )
placeLFragmentsOnLines accLine accLines lFragments =
    let
        addLastFragment f =
            { accLine | fragments = f :: accLine.fragments |> List.reverse }
    in
    case lFragments of
        [] ->
            ( accLine, accLines )

        [ f ] ->
            -- Last or single fragment = no line break.
            placeLFragmentsOnLines
                { accLine | fragments = f :: accLine.fragments }
                accLines
                []

        f :: fNext :: restLFragments ->
            -- Two fragments = line break.
            placeLFragmentsOnLines
                emptyLine
                (addLastFragment f :: accLines)
                (fNext :: restLFragments)


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
