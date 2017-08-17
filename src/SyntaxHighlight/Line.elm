module SyntaxHighlight.Line exposing (..)


type alias Line =
    { fragments : List Fragment
    , isHighlight : Bool
    }


newLine : List Fragment -> Line
newLine fragments =
    { fragments = fragments
    , isHighlight = False
    }


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
