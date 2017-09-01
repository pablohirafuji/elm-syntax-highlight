module SyntaxHighlight.Language.Css
    exposing
        ( parse
          -- Exposing just for tests purpose
        , toSyntax
          -- Exposing just for tests purpose
        , SyntaxType(..)
        )

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed)
import SyntaxHighlight.Line exposing (Line, Fragment, Color(..))
import SyntaxHighlight.Line.Helpers exposing (toLines, normal, emphasis)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, whitespaceCharSet, isSpace, isLineBreak, delimited, thenIgnore, consThen, addThen, escapable, isEscapable)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | String
    | Selector Selector
    | Property
    | PropertyValue
    | Number
    | Unit
    | LineBreak


type Selector
    = Element
    | Id
    | Class
    | Combinator
    | Universal
    | AttributeSelector AttributeSelector
    | PseudoElement
    | PseudoClass


type AttributeSelector
    = AttributeName
    | AttributeValue
    | AttributeOperator


parse : String -> Result Error (List Line)
parse =
    toSyntax
        >> Result.map (toLines LineBreak toFragment)


toSyntax : String -> Result Error (List Syntax)
toSyntax =
    mainLoop
        |> repeat zeroOrMore
        |> map (List.reverse >> List.concat)
        |> Parser.run


mainLoop : Parser (List Syntax)
mainLoop =
    oneOf
        [ whitespaceOrComment
        , selector
        , declarationBlock
        , keep oneOrMore (\c -> c == ',' || c == '}' || c == '/' || c == ']')
            |> map ((,) Normal >> List.singleton)
        ]



-- Selectors


selector : Parser (List Syntax)
selector =
    oneOf
        [ oneOf
            [ id
            , class
            , element
            , universal
            , combinator
            , pseudoElement
            , pseudoClass
            ]
            |> map (Tuple.mapFirst Selector >> List.singleton)
        , attributeSelector
        ]


id : Parser ( Selector, String )
id =
    symbol "#"
        |> thenIgnore zeroOrMore isSelectorNameChar
        |> source
        |> map ((,) Id)


class : Parser ( Selector, String )
class =
    symbol "."
        |> thenIgnore zeroOrMore isSelectorNameChar
        |> source
        |> map ((,) Class)


element : Parser ( Selector, String )
element =
    keep oneOrMore isSelectorNameChar
        |> map ((,) Element)


universal : Parser ( Selector, String )
universal =
    symbol "*"
        |> map (always ( Universal, "*" ))


combinator : Parser ( Selector, String )
combinator =
    oneOf
        [ symbol "+"
        , symbol "~"
        , symbol ">"
        ]
        |> source
        |> map ((,) Combinator)


pseudoElement : Parser ( Selector, String )
pseudoElement =
    symbol "::"
        |> thenIgnore zeroOrMore isSelectorNameChar
        |> source
        |> map ((,) PseudoElement)


pseudoClass : Parser ( Selector, String )
pseudoClass =
    symbol ":"
        |> thenIgnore zeroOrMore isSelectorNameChar
        |> source
        |> map ((,) PseudoClass)


isSelectorNameChar : Char -> Bool
isSelectorNameChar c =
    isWhitespace c
        || isCommentChar c
        || Set.member c selectorNameInvalidCharSet
        |> not


selectorNameInvalidCharSet : Set Char
selectorNameInvalidCharSet =
    Set.fromList [ ':', '{', '}', ',', '.', '#', '>', '+', '~', '*', '[', ']' ]


attributeSelector : Parser (List Syntax)
attributeSelector =
    symbol "["
        |> map (always ( Normal, "[" ))
        |> andThen
            (\opener ->
                repeat zeroOrMore attributeSelectorLoop
                    |> map
                        ((::) [ opener ]
                            >> List.reverse
                            >> List.concat
                        )
            )


attributeSelectorLoop : Parser (List Syntax)
attributeSelectorLoop =
    oneOf
        [ whitespaceOrComment
        , attributeName |> map List.singleton
        , attributeOperator
            |> andThen
                (\operator ->
                    attributeValue []
                        |> map (flip (++) [ operator ])
                )
        ]


attributeName : Parser Syntax
attributeName =
    keep oneOrMore (\c -> not <| Set.member c attSelNameInvalidCharSet)
        |> map ((,) (Selector (AttributeSelector AttributeName)))


attSelNameInvalidCharSet : Set Char
attSelNameInvalidCharSet =
    Set.union attSelOperatorCharSet whitespaceCharSet
        |> Set.insert ']'


attSelOperatorCharSet : Set Char
attSelOperatorCharSet =
    Set.fromList [ '=', '~', '|', '^', '$', '*' ]


attributeOperator : Parser Syntax
attributeOperator =
    oneOf
        [ symbol "~="
        , symbol "|="
        , symbol "^="
        , symbol "$="
        , symbol "*="
        , symbol "="
        ]
        |> source
        |> map ((,) (Selector (AttributeSelector AttributeOperator)))


attributeValue : List Syntax -> Parser (List Syntax)
attributeValue revSyntaxes =
    oneOf
        [ whitespaceOrComment
            |> addThen attributeValue revSyntaxes
        , stringLiteral
            |> addThen succeed revSyntaxes
        , keep oneOrMore (\c -> c /= ']' && not (isWhitespace c))
            |> map ((,) (Selector (AttributeSelector AttributeValue)))
            |> consThen succeed revSyntaxes
        , succeed revSyntaxes
        ]



-- Declaration Block


declarationBlock : Parser (List Syntax)
declarationBlock =
    keep oneOrMore ((==) '{')
        |> map ((,) Normal)
        |> andThen declarationBlockHelper


declarationBlockHelper : Syntax -> Parser (List Syntax)
declarationBlockHelper opener =
    repeat zeroOrMore declarationLoop
        |> map
            ((::) [ opener ]
                >> List.reverse
                >> List.concat
            )


declarationLoop : Parser (List Syntax)
declarationLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isPropertyChar
            |> map ((,) Property >> List.singleton)
        , keep oneOrMore (\c -> c == ';' || c == '/')
            |> map ((,) Normal >> List.singleton)
        , value
        ]


isPropertyChar : Char -> Bool
isPropertyChar c =
    not (isWhitespace c || isCommentChar c || c == ':' || c == ';' || c == '}')


value : Parser (List Syntax)
value =
    keep oneOrMore ((==) ':')
        |> map ((,) Normal)
        |> andThen valueHelper


valueHelper : Syntax -> Parser (List Syntax)
valueHelper opener =
    repeat zeroOrMore valueLoop
        |> map
            ((::) [ opener ]
                >> List.reverse
                >> List.concat
            )


valueLoop : Parser (List Syntax)
valueLoop =
    oneOf
        [ whitespaceOrComment
        , stringLiteral
        , number |> map List.singleton
        , hexColor
        , stringArg "url"
        , stringArg "format"
        , stringArg "local"
        , keep oneOrMore isPropertyValueChar
            |> map
                (\n ->
                    if isUnit n then
                        [ ( Unit, n ) ]
                    else
                        [ ( PropertyValue, n ) ]
                )
        , keep oneOrMore isNotPropertyValueChar
            |> map ((,) Normal >> List.singleton)
        , keep oneOrMore isOperatorChar
            |> map ((,) Unit >> List.singleton)
        ]


hexColor : Parser (List Syntax)
hexColor =
    symbol "#"
        |> andThen
            (\_ ->
                keep zeroOrMore isPropertyValueChar
                    |> map (\n -> [ ( Number, "#" ++ n ) ])
            )


stringArg : String -> Parser (List Syntax)
stringArg fnStr =
    keyword (fnStr ++ "(")
        |> map (always [ ( Normal, "(" ), ( PropertyValue, fnStr ) ])
        |> andThen
            (\opener ->
                oneOf
                    [ stringLiteral
                        |> map (\ns -> ns ++ opener)
                    , keep zeroOrMore ((/=) ')')
                        |> map (\n -> ( String, n ) :: opener)
                    ]
            )


isPropertyValueChar : Char -> Bool
isPropertyValueChar c =
    isPropertyChar c && not (c == '(' || c == ')' || c == ',' || isOperatorChar c)


isNotPropertyValueChar : Char -> Bool
isNotPropertyValueChar c =
    c == '(' || c == ')' || c == ':' || c == ',' || c == '/'


isUnit : String -> Bool
isUnit n =
    Set.member n unitSet


unitSet : Set String
unitSet =
    Set.fromList
        [ "em"
        , "ex"
        , "ch"
        , "rem"
        , "vw"
        , "vh"
        , "vmin"
        , "vmax"
        , "cm"
        , "mm"
        , "q"
        , "in"
        , "pt"
        , "pc"
        , "px"
        , "deg"
        , "grad"
        , "rad"
        , "turn"
        , "s"
        , "ms"
        , "Hz"
        , "kHz"
        , "dpi"
        , "dpcm"
        , "dppx"
        ]


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c operatorCharSet


operatorCharSet : Set Char
operatorCharSet =
    Set.fromList
        [ '+'
        , '-'
        , '%'
        , '*'
        , '/'
        ]



-- String literal


stringLiteral : Parser (List Syntax)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
        ]


quote : Parser (List Syntax)
quote =
    delimited quoteDelimiter


quoteDelimiter : Delimiter Syntax
quoteDelimiter =
    { start = "'"
    , end = "'"
    , isNestable = False
    , defaultMap = ((,) String)
    , innerParsers = [ lineBreak, cssEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Syntax)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\''



-- Comment


comment : Parser (List Syntax)
comment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = ((,) Comment)
        , innerParsers = [ lineBreak ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '/'



-- Helpers


whitespaceOrComment : Parser (List Syntax)
whitespaceOrComment =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) Normal >> List.singleton)
        , lineBreak
        , comment
        ]


lineBreak : Parser (List Syntax)
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) LineBreak)
        |> repeat oneOrMore


number : Parser Syntax
number =
    SyntaxHighlight.Helpers.number
        |> source
        |> map ((,) Number)


cssEscapable : Parser (List Syntax)
cssEscapable =
    escapable
        |> source
        |> map ((,) Number)
        |> repeat oneOrMore


toFragment : Syntax -> Fragment
toFragment ( syntaxType, text ) =
    case syntaxType of
        Normal ->
            normal Default text

        Comment ->
            normal Color1 text

        String ->
            normal Color2 text

        Selector selector ->
            selectorToFragment selector text

        Property ->
            emphasis Color4 text

        PropertyValue ->
            normal Color4 text

        Number ->
            normal Color6 text

        Unit ->
            normal Color3 text

        LineBreak ->
            normal Default text


selectorToFragment : Selector -> String -> Fragment
selectorToFragment selector text =
    case selector of
        Element ->
            normal Color3 text

        Id ->
            normal Color5 text

        Class ->
            normal Color5 text

        Combinator ->
            normal Color7 text

        Universal ->
            normal Color3 text

        AttributeSelector att ->
            attributeSelectorToFragment att text

        PseudoElement ->
            normal Default text

        PseudoClass ->
            normal Default text


attributeSelectorToFragment : AttributeSelector -> String -> Fragment
attributeSelectorToFragment att text =
    case att of
        AttributeName ->
            normal Color5 text

        AttributeValue ->
            normal Color2 text

        AttributeOperator ->
            normal Color3 text
