module SyntaxHighlight.Language.Css
    exposing
        ( toLines
        , Syntax(..)
        , AtRule(..)
        , Selector(..)
        , AttributeSelector(..)
        , syntaxToStyle
          -- Exposing for tests purpose
        , toRevTokens
        )

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen, repeat, succeed)
import SyntaxHighlight.Language.Type as T
import SyntaxHighlight.Language.Helpers exposing (Delimiter, isWhitespace, whitespaceCharSet, isSpace, isLineBreak, delimited, thenIgnore, consThen, addThen, escapable, isEscapable)
import SyntaxHighlight.Line exposing (Line)
import SyntaxHighlight.Line.Helpers as Line
import SyntaxHighlight.Style as Style exposing (Required(..))


type alias Token =
    T.Token Syntax


type Syntax
    = String
    | AtRule AtRule
    | Selector Selector
    | Property
    | PropertyValue
    | Number
    | Unit


type AtRule
    = Identifier
    | Prefix
    | Keyword
    | AtRuleValue


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


toLines : String -> Result Error (List Line)
toLines =
    toRevTokens
        >> Result.map (Line.toLines syntaxToStyle)


toRevTokens : String -> Result Error (List Token)
toRevTokens =
    mainLoop
        |> repeat zeroOrMore
        |> map (List.reverse >> List.concat)
        |> Parser.run


mainLoop : Parser (List Token)
mainLoop =
    oneOf
        [ whitespaceOrComment
        , atRule
        , selector
        , declarationBlock
        , keep (Exactly 1) (always True)
            |> map ((,) T.Normal >> List.singleton)
        ]



-- At-Rules


atRule : Parser (List Token)
atRule =
    symbol "@"
        |> thenIgnore zeroOrMore isSelectorNameChar
        |> source
        |> andThen atRuleHelper


atRuleHelper : String -> Parser (List Token)
atRuleHelper atRule =
    case atRule of
        "@import" ->
            oneOf
                [ whitespaceOrComment
                , stringArg "url"
                , stringLiteral
                , atRuleKeywordOrValue
                , keep (Exactly 1) (\c -> c /= ';')
                    |> map ((,) T.Normal >> List.singleton)
                ]
                |> repeat zeroOrMore
                |> map (finishAtRules atRule)

        "@namespace" ->
            oneOf
                [ whitespaceOrComment
                , stringArg "url"
                , stringLiteral
                , keep oneOrMore isSelectorNameChar
                    |> map ((,) (T.C (AtRule Prefix)) >> List.singleton)
                , keep (Exactly 1) (\c -> c /= ';')
                    |> map ((,) T.Normal >> List.singleton)
                ]
                |> repeat zeroOrMore
                |> map (finishAtRules atRule)

        "@charset" ->
            oneOf
                [ whitespaceOrComment
                , stringLiteral
                , keep oneOrMore isSelectorNameChar
                    |> map ((,) (T.C String) >> List.singleton)
                , keep (Exactly 1) (\c -> c /= ';')
                    |> map ((,) T.Normal >> List.singleton)
                ]
                |> repeat zeroOrMore
                |> map (finishAtRules atRule)

        "@media" ->
            mediaOrSupports atRule

        "@supports" ->
            mediaOrSupports atRule

        "@keyframes" ->
            keyframesOrCounterStyle atRule
                |> andThen nestableAtRuleOpener

        "@counter-style" ->
            keyframesOrCounterStyle atRule

        "@font-feature-values" ->
            oneOf
                [ whitespaceOrComment
                , keep oneOrMore isSelectorNameChar
                    |> map ((,) (T.C (AtRule Prefix)) >> List.singleton)
                , keep (Exactly 1) (\c -> c /= '{')
                    |> map ((,) T.Normal >> List.singleton)
                ]
                |> repeat zeroOrMore
                |> map (finishAtRules atRule)
                |> andThen nestableAtRuleOpener

        _ ->
            if Set.member atRule atRuleSet then
                succeed [ ( T.C (AtRule Identifier), atRule ) ]
            else
                succeed [ ( T.Normal, atRule ) ]


atRuleKeywordOrValue : Parser (List Token)
atRuleKeywordOrValue =
    keep oneOrMore isSelectorNameChar
        |> map
            (\n ->
                if isAtRuleKeyword n then
                    [ ( T.C (AtRule Keyword), n ) ]
                else
                    [ ( T.C (AtRule AtRuleValue), n ) ]
            )


mediaOrSupports : String -> Parser (List Token)
mediaOrSupports atRule =
    oneOf
        [ whitespaceOrComment
        , stringLiteral
        , atRuleKeywordOrValue
        , keep (Exactly 1) (\c -> c /= '{')
            |> map ((,) T.Normal >> List.singleton)
        ]
        |> repeat zeroOrMore
        |> map (finishAtRules atRule)
        |> andThen nestableAtRuleOpener


keyframesOrCounterStyle : String -> Parser (List Token)
keyframesOrCounterStyle atRule =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isSelectorNameChar
            |> map ((,) (T.C (AtRule Prefix)) >> List.singleton)
        , keep (Exactly 1) (\c -> c /= '{')
            |> map ((,) T.Normal >> List.singleton)
        ]
        |> repeat zeroOrMore
        |> map (finishAtRules atRule)


finishAtRules : String -> List (List Token) -> List Token
finishAtRules atRule ns =
    [ ( T.C (AtRule Identifier), atRule ) ]
        :: ns
        |> List.reverse
        |> List.concat


nestableAtRuleOpener : List Token -> Parser (List Token)
nestableAtRuleOpener ns =
    oneOf
        [ symbol "{"
            |> map (always (( T.Normal, "{" ) :: ns))
        , succeed ns
        ]


isAtRuleKeyword : String -> Bool
isAtRuleKeyword n =
    Set.member n atRuleKeywordSet


atRuleKeywordSet : Set String
atRuleKeywordSet =
    Set.fromList
        [ "and"
        , "or"
        , "not"
        , "only"
        ]


atRuleSet : Set String
atRuleSet =
    Set.fromList
        [ "@page"
        , "@font-face"

        -- Font-feature-values at-rules
        , "@swash"
        , "@annotation"
        , "@ornaments"
        , "@stylistic"
        , "@styleset"
        , "@character-variant"
        ]



-- Selectors


selector : Parser (List Token)
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
            |> map (Tuple.mapFirst (T.C << Selector) >> List.singleton)
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
    Set.fromList [ ':', '{', '}', ',', '.', '#', '>', '+', '~', '*', '[', ']', '|', ';', '(', ')' ]


attributeSelector : Parser (List Token)
attributeSelector =
    symbol "["
        |> map (always ( T.Normal, "[" ))
        |> andThen
            (\opener ->
                repeat zeroOrMore attributeSelectorLoop
                    |> map
                        ((::) [ opener ]
                            >> List.reverse
                            >> List.concat
                        )
            )


attributeSelectorLoop : Parser (List Token)
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


attributeName : Parser Token
attributeName =
    keep oneOrMore (\c -> not <| Set.member c attSelNameInvalidCharSet)
        |> map ((,) (T.C (Selector (AttributeSelector AttributeName))))


attSelNameInvalidCharSet : Set Char
attSelNameInvalidCharSet =
    Set.union attSelOperatorCharSet whitespaceCharSet
        |> Set.insert ']'


attSelOperatorCharSet : Set Char
attSelOperatorCharSet =
    Set.fromList [ '=', '~', '|', '^', '$', '*' ]


attributeOperator : Parser Token
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
        |> map ((,) (T.C (Selector (AttributeSelector AttributeOperator))))


attributeValue : List Token -> Parser (List Token)
attributeValue revSyntaxes =
    oneOf
        [ whitespaceOrComment
            |> addThen attributeValue revSyntaxes
        , stringLiteral
            |> addThen succeed revSyntaxes
        , keep oneOrMore (\c -> c /= ']' && not (isWhitespace c))
            |> map ((,) (T.C (Selector (AttributeSelector AttributeValue))))
            |> consThen succeed revSyntaxes
        , succeed revSyntaxes
        ]



-- Declaration Block


declarationBlock : Parser (List Token)
declarationBlock =
    keep oneOrMore ((==) '{')
        |> map ((,) T.Normal)
        |> andThen declarationBlockHelper


declarationBlockHelper : Token -> Parser (List Token)
declarationBlockHelper opener =
    repeat zeroOrMore declarationLoop
        |> map
            ((::) [ opener ]
                >> List.reverse
                >> List.concat
            )


declarationLoop : Parser (List Token)
declarationLoop =
    oneOf
        [ whitespaceOrComment
        , keep oneOrMore isPropertyChar
            |> map ((,) (T.C Property) >> List.singleton)
        , keep oneOrMore (\c -> c == ';' || c == '/')
            |> map ((,) T.Normal >> List.singleton)
        , value
        ]


isPropertyChar : Char -> Bool
isPropertyChar c =
    not (isWhitespace c || isCommentChar c || c == ':' || c == ';' || c == '}')


value : Parser (List Token)
value =
    keep oneOrMore ((==) ':')
        |> map ((,) T.Normal)
        |> andThen valueHelper


valueHelper : Token -> Parser (List Token)
valueHelper opener =
    repeat zeroOrMore valueLoop
        |> map
            ((::) [ opener ]
                >> List.reverse
                >> List.concat
            )


valueLoop : Parser (List Token)
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
                        [ ( T.C Unit, n ) ]
                    else
                        [ ( T.C PropertyValue, n ) ]
                )
        , keep oneOrMore isNotPropertyValueChar
            |> map ((,) T.Normal >> List.singleton)
        , keep oneOrMore isOperatorChar
            |> map ((,) (T.C Unit) >> List.singleton)
        ]


hexColor : Parser (List Token)
hexColor =
    --SyntaxHighlight.Helpers.hexColor
    --    |> source
    --    |> map ((,) Number >> List.singleton)
    symbol "#"
        |> andThen
            (\_ ->
                keep zeroOrMore isPropertyValueChar
                    |> map (\n -> [ ( T.C Number, "#" ++ n ) ])
            )


stringArg : String -> Parser (List Token)
stringArg fnStr =
    keyword (fnStr ++ "(")
        |> map (always [ ( T.Normal, "(" ), ( T.C PropertyValue, fnStr ) ])
        |> andThen
            (\opener ->
                oneOf
                    [ stringLiteral
                        |> map (\ns -> ns ++ opener)
                    , keep zeroOrMore ((/=) ')')
                        |> map (\n -> ( T.C String, n ) :: opener)
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


stringLiteral : Parser (List Token)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
        ]


quote : Parser (List Token)
quote =
    delimited quoteDelimiter


quoteDelimiter : Delimiter Token
quoteDelimiter =
    { start = "'"
    , end = "'"
    , isNestable = False
    , defaultMap = ((,) (T.C String))
    , innerParsers = [ lineBreak, cssEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Token)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\''



-- T.Comment


comment : Parser (List Token)
comment =
    delimited
        { start = "/*"
        , end = "*/"
        , isNestable = False
        , defaultMap = ((,) T.Comment)
        , innerParsers = [ lineBreak ]
        , isNotRelevant = \c -> not (isLineBreak c)
        }


isCommentChar : Char -> Bool
isCommentChar c =
    c == '/'



-- Helpers


whitespaceOrComment : Parser (List Token)
whitespaceOrComment =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) T.Normal >> List.singleton)
        , lineBreak
        , comment
        ]


lineBreak : Parser (List Token)
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) T.LineBreak)
        |> repeat oneOrMore


number : Parser Token
number =
    SyntaxHighlight.Language.Helpers.number
        |> source
        |> map ((,) (T.C Number))


cssEscapable : Parser (List Token)
cssEscapable =
    escapable
        |> source
        |> map ((,) (T.C Number))
        |> repeat oneOrMore


syntaxToStyle : Syntax -> ( Style.Required, String )
syntaxToStyle syntax =
    case syntax of
        String ->
            ( Style2, "css-s" )

        AtRule atRule ->
            atRuleToFragment atRule

        Selector selector ->
            selectorToFragment selector

        Property ->
            ( Style4, "css-p" )

        PropertyValue ->
            ( Style4, "css-pv" )

        Number ->
            ( Style1, "css-n" )

        Unit ->
            ( Style3, "css-u" )


atRuleToFragment : AtRule -> ( Style.Required, String )
atRuleToFragment atRule =
    case atRule of
        Identifier ->
            ( Style3, "css-ar-i" )

        Prefix ->
            ( Style5, "css-ar-p" )

        Keyword ->
            ( Style3, "css-ar-k" )

        AtRuleValue ->
            ( Style4, "css-ar-v" )


selectorToFragment : Selector -> ( Style.Required, String )
selectorToFragment selector =
    case selector of
        Element ->
            ( Style3, "css-s-e" )

        Id ->
            ( Style5, "css-s-i" )

        Class ->
            ( Style5, "css-s-cl" )

        Combinator ->
            ( Style7, "css-s-c" )

        Universal ->
            ( Style3, "css-s-u" )

        AttributeSelector att ->
            attributeSelectorToFragment att

        PseudoElement ->
            ( Default, "css-s-pe" )

        PseudoClass ->
            ( Default, "css-s-pc" )


attributeSelectorToFragment : AttributeSelector -> ( Style.Required, String )
attributeSelectorToFragment att =
    case att of
        AttributeName ->
            ( Style5, "css-s-a-an" )

        AttributeValue ->
            ( Style2, "css-s-a-av" )

        AttributeOperator ->
            ( Style3, "css-s-a-o" )
