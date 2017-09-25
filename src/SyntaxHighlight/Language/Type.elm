module SyntaxHighlight.Language.Type exposing (Token, Syntax(..))


type alias Token a =
    ( Syntax a, String )


type Syntax a
    = Normal
    | Comment
    | LineBreak
    | C a
