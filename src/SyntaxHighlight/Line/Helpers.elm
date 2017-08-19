module SyntaxHighlight.Line.Helpers
    exposing
        ( toLines
        , normal
        , emphasis
        , strong
        , strongEmphasis
        )

import SyntaxHighlight.Line exposing (Line, Fragment, Color(Default))


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


newLine : List Fragment -> Line
newLine fragments =
    { fragments = fragments
    , highlight = Nothing
    }


normal : Color -> String -> Fragment
normal color text =
    { text = text
    , color = color
    , isEmphasis = False
    , isStrong = False
    }


emphasis : Color -> String -> Fragment
emphasis color text =
    { text = text
    , color = color
    , isEmphasis = True
    , isStrong = False
    }


strong : Color -> String -> Fragment
strong color text =
    { text = text
    , color = color
    , isEmphasis = False
    , isStrong = True
    }


strongEmphasis : Color -> String -> Fragment
strongEmphasis color text =
    { text = text
    , color = color
    , isEmphasis = True
    , isStrong = True
    }
