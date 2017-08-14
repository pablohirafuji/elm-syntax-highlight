module SyntaxHighlight.Fragment exposing (..)


type alias Fragment =
    { color : Color
    , isEmphasis : Bool
    , isStrong : Bool
    , text : String
    }



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
    { color = color
    , isEmphasis = False
    , isStrong = False
    , text = text
    }


emphasis : Color -> String -> Fragment
emphasis color text =
    { color = color
    , isEmphasis = True
    , isStrong = False
    , text = text
    }


strong : Color -> String -> Fragment
strong color text =
    { color = color
    , isEmphasis = False
    , isStrong = True
    , text = text
    }


strongEmphasis : Color -> String -> Fragment
strongEmphasis color text =
    { color = color
    , isEmphasis = True
    , isStrong = True
    , text = text
    }
