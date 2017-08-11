module SyntaxHighlight.Style exposing (..)


type alias Style =
    { color : Color
    , isItalic : Bool
    , isBold : Bool
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
    , isItalic = False
    , isBold = False
    }


italic : Color -> Style
italic color =
    { color = color
    , isItalic = True
    , isBold = False
    }


bold : Color -> Style
bold color =
    { color = color
    , isItalic = False
    , isBold = True
    }


italicBold : Color -> Style
italicBold color =
    { color = color
    , isItalic = True
    , isBold = True
    }
