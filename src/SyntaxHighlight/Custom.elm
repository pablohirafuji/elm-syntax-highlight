module SyntaxHighlight.Custom exposing
    ( fromParser
    , Fragment
    , fragment, newline, setFragmentStyle, setFragmentClasses
    , Style
    , styleDefault, styleComment, style1, style2, style3, style4, style5, style6, style7
    )

{-| You can highlight your own custom syntax by using this module!

@docs fromParser


## Fragments

@docs Fragment

@docs fragment, newline, setFragmentStyle, setFragmentClasses


## Styles

@docs Style

@docs styleDefault, styleComment, style1, style2, style3, style4, style5, style6, style7

-}

import Parser exposing (Parser)
import SyntaxHighlight exposing (Highlight(..))
import SyntaxHighlight.Line as Line exposing (Line)
import SyntaxHighlight.Style as Style


{-| Use a parser from [`elm/parser`][elm-parser] to define your own custom
syntax. Your parser must produce a list of [`Fragment`](#Fragment) values out of
the code string.

Here's a small example, to give you an idea of how to write your syntax. You can
also check the [custom syntax example][example] in the repo.

[elm-parser]: https://package.elm-lang.org/packages/elm/parser/latest/
[example]: https://github.com/pablohirafuji/elm-syntax-highlight/blob/3.8.0/demo/src/CustomSyntax.elm

    import Html
    import Parser exposing (Parser)
    import SyntaxHighlight
    import SyntaxHighlight.Custom as Custom

    viewHighlightedCustomSyntax : String -> Html msg
    viewHighlightedCustomSyntax code =
        code
            |> Custom.fromParser parser
            |> Result.map (SyntaxHighlight.toBlockHtml Nothing)
            |> Result.withDefault (Html.text "[Parser error]")

    parser : Parser (List Custom.Fragment)
    parser =
        -- This is just to demonstrate the kind of output
        -- your parser can have. Please check the `elm/parser`
        -- package to learn how to write a proper parser.
        Parser.succeed
            [ Custom.fragment "2"
                |> Custom.setFragmentStyle Custom.style1
            , Custom.fragment " "
            , Custom.fragment "+"
                |> Custom.setFragmentStyle Custom.style3
                |> Custom.setFragmentClasses "plus"
            , Custom.fragment " "
            , Custom.fragment "3"
                |> Custom.setFragmentStyle Custom.style1
            ]

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


{-| One single styled portion of custom parsed code. Construct using
[`fragment`](#fragment) or [`newline`](#newline), and use it as the returned
value of your parser when using [`fromParser`](#fromParser).
-}
type Fragment
    = Fragment Line.Fragment
    | Newline (Maybe Highlight)


{-| Constructs a `Fragment` value out of a part of the code you're parsing.
Typically should represent one syntactic unit in the code, such as a string,
an identifier, a keyword, an operator, whitespace, etc. You can add styling
details to this `Fragment` using [`setFragmentStyle`](#setFragmentStyle) and
[`setFragmentClasses`](#setFragmentClasses).

Check the [`fromParser`](#fromParser) documentation for an example.

-}
fragment : String -> Fragment
fragment text =
    Fragment
        { text = text
        , requiredStyle = Style.Default
        , additionalClass = ""
        }


{-| A special kind of `Fragment`: a way to insert an explicit break
into a new line when parsing your code. You can optionally pass it a
[`Highlight`](SyntaxHighlight#Highlight) value to change how the following line
is rendered.

Please note that regular line breaks (`'\n'`) in a fragment are interpreted
the same way as `newline Nothing`. So you don't really need to use this at all
unless you want to highlight a line in your custom syntax code.

Here's an example of how you can mark a line as “added” using this function.

    import Parser
    import SyntaxHighlight
    import SyntaxHighlight.Custom as Custom

    parser =
        Parser.succeed
            [ Custom.fragment "first line"
            , Custom.newline (Some SyntaxHighlight.Add)
            , Custom.fragment "added line"
            , Custom.newline Nothing
            , Custom.fragment "normal line"
            ]

-}
newline : Maybe Highlight -> Fragment
newline hl =
    Newline hl


{-| Sets a specific [`Style`](#Style) to a [`Fragment`](#Fragment), which gives
it a different color depending on the theme used.
-}
setFragmentStyle : Style -> Fragment -> Fragment
setFragmentStyle (Style style) f =
    case f of
        Fragment lf ->
            Fragment { lf | requiredStyle = style }

        Newline _ ->
            f


{-| You can optionally use this function to give a [`Fragment`](#Fragment) one
or more custom CSS classes (separated by spaces), if you want more control over
how you style your custom syntax.
-}
setFragmentClasses : String -> Fragment -> Fragment
setFragmentClasses classes f =
    case f of
        Fragment lf ->
            Fragment { lf | additionalClass = classes }

        Newline _ ->
            f



-- STYLE


{-| The style of a `Fragment`, whose actual appearance is given by the theme
that is applied. Construct using the different `style*` definitions provided,
and set it to a fragment using [`setFragmentStyle`](#setFragmentStyle).

    import SyntaxHighlight.Custom as Custom

    aKeywordFragment =
        Custom.fragment "case"
            |> Custom.setFragmentStyle Custom.style3

-}
type Style
    = Style Style.Required


{-| The default style for a fragment.
-}
styleDefault : Style
styleDefault =
    Style Style.Default


{-| Style specific for comments.
-}
styleComment : Style
styleComment =
    Style Style.Comment


{-| Style typically used for numbers.
-}
style1 : Style
style1 =
    Style Style.Style1


{-| Style typically used for literal strings and attribute values.
-}
style2 : Style
style2 =
    Style Style.Style2


{-| Style typically used for keywords, tags and operator symbols (`=+-*/`…).
-}
style3 : Style
style3 =
    Style Style.Style3


{-| Style typically used for keywords (alternative), group symbols (`{}(),`) and type signatures.
-}
style4 : Style
style4 =
    Style Style.Style4


{-| Style typically used for functions and attribute names.
-}
style5 : Style
style5 =
    Style Style.Style5


{-| Style typically used for literal keywords and capitalized types.
-}
style6 : Style
style6 =
    Style Style.Style6


{-| Style typically used for arguments and parameters.
-}
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
