module SyntaxHighlight.Language.Type exposing (HCode(..), Syntax(..), Token)

import SyntaxHighlight.Line exposing (Line)


type HCode
    = HCode (List Line)


type alias Token a =
    ( Syntax a, String )


type Syntax a
    = Normal
    | Comment
    | LineBreak
    | C a
