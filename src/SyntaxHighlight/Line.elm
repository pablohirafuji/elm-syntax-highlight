module SyntaxHighlight.Line
    exposing
        ( Line
        , newLine
        , toLines
        , highlightLines
        , Fragment
        , Color(..)
        , normal
        , emphasis
        , strong
        , strongEmphasis
        )


type alias Line =
    { fragments : List Fragment
    , isHighlight : Bool
    }


newLine : List Fragment -> Line
newLine fragments =
    { fragments = fragments
    , isHighlight = False
    }


toLines : a -> (( a, String ) -> Fragment) -> List ( a, String ) -> List Line
toLines lineBreak toFragment revSyntaxes =
    List.foldl (toLinesHelp lineBreak toFragment) ( [], [], Nothing ) revSyntaxes
        |> (\( lines, frags, _ ) -> newLine frags :: lines)


toLinesHelp : a -> (( a, String ) -> Fragment) -> ( a, String ) -> ( List Line, List Fragment, Maybe a ) -> ( List Line, List Fragment, Maybe a )
toLinesHelp lineBreak toFragment ( syntaxType, text ) ( lines, fragments, maybeLastType ) =
    if syntaxType == lineBreak then
        ( newLine fragments :: lines
        , [ normal Default text ]
        , Nothing
        )
    else if Just syntaxType == maybeLastType then
        -- Concat same syntax sequence to reduce html elements.
        case fragments of
            headFrag :: tailFrags ->
                ( lines
                , { headFrag | text = text ++ headFrag.text } :: tailFrags
                , maybeLastType
                )

            _ ->
                ( lines
                , toFragment ( syntaxType, text ) :: fragments
                , maybeLastType
                )
    else
        ( lines
        , toFragment ( syntaxType, text ) :: fragments
        , Just syntaxType
        )


highlightLines : Int -> Int -> List Line -> List Line
highlightLines start end lines =
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
        List.indexedMap (highlightLinesHelp start_ end_) lines


highlightLinesHelp : Int -> Int -> Int -> Line -> Line
highlightLinesHelp start end index line =
    if index >= start && index < end then
        { line | isHighlight = True }
    else
        line


type alias Fragment =
    { text : String
    , color : Color
    , isEmphasis : Bool
    , isStrong : Bool
    , isHighlight : Bool
    }



-- Style
-- The comment say the most common uses of the color.


type Color
    = Default
    | Color1 -- Comment
    | Color2 -- Literal string, attribute value
    | Color3 -- Keyword, tag, operator symbol (=+-*/...)
    | Color4 -- Keyword, type signature, group symbol ({}(),)
    | Color5 -- Function, attribute name
    | Color6 -- Literal keyword
    | Color7 -- Argument, parameter


normal : Color -> String -> Fragment
normal color text =
    { text = text
    , color = color
    , isEmphasis = False
    , isStrong = False
    , isHighlight = False
    }


emphasis : Color -> String -> Fragment
emphasis color text =
    { text = text
    , color = color
    , isEmphasis = True
    , isStrong = False
    , isHighlight = False
    }


strong : Color -> String -> Fragment
strong color text =
    { text = text
    , color = color
    , isEmphasis = False
    , isStrong = True
    , isHighlight = False
    }


strongEmphasis : Color -> String -> Fragment
strongEmphasis color text =
    { text = text
    , color = color
    , isEmphasis = True
    , isStrong = True
    , isHighlight = False
    }
