module SyntaxHighlight.Line
    exposing
        ( Line
        , Highlight(..)
        , highlightLines
        , Fragment
        , Color(..)
        )

{-| A parsed highlighted line.

@docs Line, Fragment, Color, Highlight


## Helpers

@docs highlightLines

-}


{-| A line.
-}
type alias Line =
    { fragments : List Fragment
    , highlight : Maybe Highlight
    }


{-| A fragment.
-}
type alias Fragment =
    { text : String
    , color : Color
    , isEmphasis : Bool
    , isStrong : Bool
    }


{-| Highlight type.
-}
type Highlight
    = Normal
    | Add
    | Delete


{-| Highlight lines given a highlight type, start and end index.
If no highlight type is given (`Nothing`), it will remove any
highlight from that lines.
Negative indexes are taken starting from the *end* of the list.
-}
highlightLines : Maybe Highlight -> Int -> Int -> List Line -> List Line
highlightLines maybeHighlight start end lines =
    let
        length =
            List.length lines

        start_ =
            if start < 0 then
                length + start
            else
                start

        end_ =
            if end < 0 then
                length + end
            else
                end
    in
        List.indexedMap (highlightLinesHelp maybeHighlight start_ end_) lines


highlightLinesHelp : Maybe Highlight -> Int -> Int -> Int -> Line -> Line
highlightLinesHelp maybeHighlight start end index line =
    if index >= start && index < end then
        { line | highlight = maybeHighlight }
    else
        line


{-| Possible colors.

The common uses of the color are the following:

  - Default: Default color
  - Color1: Comment
  - Color2: Literal string, attribute value
  - Color3: Keyword, tag, operator symbol (=+-*/...)
  - Color4: Keyword, type signature, group symbol ({}(),)
  - Color5: Function, attribute name
  - Color6: Literal keyword, number
  - Color7: Argument, parameter

-}
type Color
    = Default
    | Color1
    | Color2
    | Color3
    | Color4
    | Color5
    | Color6
    | Color7
