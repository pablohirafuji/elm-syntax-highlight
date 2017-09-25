module SyntaxHighlight.Line
    exposing
        ( Line
        , Highlight(..)
        , highlightLines
        , Fragment
        )

{-| A parsed highlighted line.

@docs Line, Fragment, Highlight


## Helpers

@docs highlightLines

-}

import SyntaxHighlight.Style as Style


{-| A line holds information about its fragments and if is highlighted in any way.
-}
type alias Line =
    { fragments : List Fragment
    , highlight : Maybe Highlight
    }


{-| A fragment holds information about the text being styled, the style and additional class to be applied.
-}
type alias Fragment =
    { text : String
    , requiredStyle : Style.Required
    , additionalClass : String
    }


{-| Highlight type. `Normal` will highlight the line in a way to differentiate it from the rest, like github's yellow background.
`Add` will highlight in a manner that gives the ideia of new content added.
`Delete` will highlight in a manner that gives the ideia of deleted content.
The specific styles will depend on the `Theme` chosen.
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
