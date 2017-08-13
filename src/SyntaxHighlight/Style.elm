module SyntaxHighlight.Style exposing (..)


type alias Style =
    { color : Color
    , isEmphasis : Bool
    , isStrong : Bool
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


normal : Color -> Style
normal color =
    { color = color
    , isEmphasis = False
    , isStrong = False
    }


emphasis : Color -> Style
emphasis color =
    { color = color
    , isEmphasis = True
    , isStrong = False
    }


strong : Color -> Style
strong color =
    { color = color
    , isEmphasis = False
    , isStrong = True
    }


strongEmphasis : Color -> Style
strongEmphasis color =
    { color = color
    , isEmphasis = True
    , isStrong = True
    }
