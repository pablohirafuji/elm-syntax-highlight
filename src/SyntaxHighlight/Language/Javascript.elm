module SyntaxHighlight.Language.Javascript exposing (parse)

import Set exposing (Set)
import Parser exposing (Parser, oneOf, zeroOrMore, oneOrMore, ignore, symbol, keyword, (|.), (|=), source, ignoreUntil, keep, Count(..), Error, map, andThen)
import SyntaxHighlight.Line exposing (Line, newLine, Fragment, Color(..), normal, emphasis)
import SyntaxHighlight.Helpers exposing (Delimiter, isWhitespace, isSpace, isLineBreak, delimited, escapable, isEscapable)


type alias Syntax =
    ( SyntaxType, String )


type SyntaxType
    = Normal
    | Comment
    | String
    | Keyword
    | DeclarationKeyword
    | FunctionEval
    | Function
    | LiteralKeyword
    | Param
    | LineBreak


parse : String -> Result Error (List Line)
parse =
    Parser.run (mainLoop [])
        >> Result.map toLines


mainLoop : List Syntax -> Parser (List Syntax)
mainLoop revSyntaxList =
    oneOf
        [ oneOf
            [ comment
            , stringLiteral
            ]
            |> andThen (\n -> mainLoop (n ++ revSyntaxList))
        , whitespace
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , keep oneOrMore isOperatorChar
            |> andThen (\n -> mainLoop (( Keyword, n ) :: revSyntaxList))
        , keep oneOrMore isGroupChar
            |> andThen
                (\n -> mainLoop (( Normal, n ) :: revSyntaxList))
        , number
            |> andThen (\n -> mainLoop (n :: revSyntaxList))
        , keep oneOrMore isIdentifierNameChar
            |> andThen
                (\n ->
                    if n == "function" || n == "static" then
                        functionStatementLoop
                            (( DeclarationKeyword, n ) :: revSyntaxList)
                    else if n == "class" then
                        classStatementLoop
                            (( DeclarationKeyword, n ) :: revSyntaxList)
                    else if n == "this" || n == "super" then
                        mainLoop (( Param, n ) :: revSyntaxList)
                    else if n == "constructor" then
                        functionStatementLoop (( Function, n ) :: revSyntaxList)
                    else if isKeyword n then
                        mainLoop (( Keyword, n ) :: revSyntaxList)
                    else if isDeclarationKeyword n then
                        mainLoop (( DeclarationKeyword, n ) :: revSyntaxList)
                    else if isLiteralKeyword n then
                        mainLoop (( LiteralKeyword, n ) :: revSyntaxList)
                    else
                        functionEvalLoop n [] revSyntaxList
                )
        , end revSyntaxList
        ]


functionStatementLoop : List Syntax -> Parser (List Syntax)
functionStatementLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> functionStatementLoop (n ++ revSyntaxList))
        , whitespace
            |> andThen (\n -> functionStatementLoop (n :: revSyntaxList))
        , keep oneOrMore isIdentifierNameChar
            |> andThen (\n -> functionStatementLoop (( Function, n ) :: revSyntaxList))
        , symbol "("
            |> andThen (\n -> argLoop (( Normal, "(" ) :: revSyntaxList))
        , mainLoop revSyntaxList
        ]


functionEvalLoop : String -> List Syntax -> List Syntax -> Parser (List Syntax)
functionEvalLoop identifier postSyntaxList preSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> functionEvalLoop identifier (n ++ postSyntaxList) preSyntaxList)
        , whitespace
            |> andThen (\n -> functionEvalLoop identifier (n :: postSyntaxList) preSyntaxList)
        , symbol "("
            |> andThen
                (\n ->
                    mainLoop
                        ((( Normal, "(" ) :: postSyntaxList)
                            ++ (( FunctionEval, identifier )
                                    :: preSyntaxList
                               )
                        )
                )
        , mainLoop (postSyntaxList ++ (( Normal, identifier ) :: preSyntaxList))
        ]


classStatementLoop : List Syntax -> Parser (List Syntax)
classStatementLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> classStatementLoop (n ++ revSyntaxList))
        , whitespace
            |> andThen (\n -> classStatementLoop (n :: revSyntaxList))
        , keep oneOrMore isIdentifierNameChar
            |> andThen (\n -> mainLoop (( Function, n ) :: revSyntaxList))
        , mainLoop revSyntaxList
        ]


argLoop : List Syntax -> Parser (List Syntax)
argLoop revSyntaxList =
    oneOf
        [ comment
            |> andThen (\n -> argLoop (n ++ revSyntaxList))
        , whitespace
            |> andThen (\n -> argLoop (n :: revSyntaxList))
        , keep oneOrMore (\c -> not (isCommentChar c || isWhitespace c || c == ',' || c == ')'))
            |> andThen (\n -> argLoop (( Param, n ) :: revSyntaxList))
        , keep oneOrMore (\c -> c == '/' || c == ',')
            |> andThen (\n -> argLoop (( Normal, n ) :: revSyntaxList))
        , symbol ")"
            |> andThen (\n -> mainLoop (( Normal, ")" ) :: revSyntaxList))
        , end revSyntaxList
        ]


isIdentifierNameChar : Char -> Bool
isIdentifierNameChar c =
    not
        (isPunctuaction c
            || isStringLiteralChar c
            || isCommentChar c
            || isWhitespace c
        )



-- Reserved Words


isKeyword : String -> Bool
isKeyword str =
    Set.member str keywordSet


keywordSet : Set String
keywordSet =
    Set.fromList
        [ "break"
        , "do"
        , "instanceof"
        , "typeof"
        , "case"
        , "else"
        , "new"
        , "catch"
        , "finally"
        , "return"
        , "void"
        , "continue"
        , "for"
        , "switch"
        , "while"
        , "debugger"
        , "this"
        , "with"
        , "default"
        , "if"
        , "throw"
        , "delete"
        , "in"
        , "try"
        , "enum"
        , "extends"
        , "export"
        , "import"
        , "implements"
        , "private"
        , "public"
        , "yield"
        , "interface"
        , "package"
        , "protected"
        ]


isDeclarationKeyword : String -> Bool
isDeclarationKeyword str =
    Set.member str declarationKeywordSet


declarationKeywordSet : Set String
declarationKeywordSet =
    Set.fromList
        [ "var"
        , "const"
        , "let"
        ]


isPunctuaction : Char -> Bool
isPunctuaction c =
    Set.member c punctuactorSet


punctuactorSet : Set Char
punctuactorSet =
    Set.union operatorSet groupSet


operatorChar : Parser Syntax
operatorChar =
    keep oneOrMore isOperatorChar
        |> map ((,) Keyword)


isOperatorChar : Char -> Bool
isOperatorChar c =
    Set.member c operatorSet


operatorSet : Set Char
operatorSet =
    Set.fromList
        [ '+'
        , '-'
        , '*'
        , '/'
        , '='
        , '!'
        , '<'
        , '>'
        , '&'
        , '|'
        , '?'
        , '^'
        , ':'
        , '~'
        , '%'
        , '.'
        ]


groupChar : Parser Syntax
groupChar =
    keep oneOrMore isGroupChar
        |> map ((,) Normal)


isGroupChar : Char -> Bool
isGroupChar c =
    Set.member c groupSet


groupSet : Set Char
groupSet =
    Set.fromList
        [ '{'
        , '}'
        , '('
        , ')'
        , '['
        , ']'
        , ','
        , ';'
        ]


isLiteralKeyword : String -> Bool
isLiteralKeyword str =
    Set.member str literalKeywordSet


literalKeywordSet : Set String
literalKeywordSet =
    [ "true"
    , "false"
    , "null"
    , "undefined"
    , "NaN"
    , "Infinity"
    ]
        |> Set.fromList



-- String literal


stringLiteral : Parser (List Syntax)
stringLiteral =
    oneOf
        [ quote
        , doubleQuote
        , templateString
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
    , innerParsers = [ lineBreak, jsEscapable ]
    , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
    }


doubleQuote : Parser (List Syntax)
doubleQuote =
    delimited
        { quoteDelimiter
            | start = "\""
            , end = "\""
        }


templateString : Parser (List Syntax)
templateString =
    delimited
        { quoteDelimiter
            | start = "`"
            , end = "`"
            , innerParsers = [ lineBreak, jsEscapable ]
            , isNotRelevant = \c -> not (isLineBreak c || isEscapable c)
        }


isStringLiteralChar : Char -> Bool
isStringLiteralChar c =
    c == '"' || c == '\'' || c == '`'



-- Comments


comment : Parser (List Syntax)
comment =
    oneOf
        [ inlineComment
        , multilineComment
        ]


inlineComment : Parser (List Syntax)
inlineComment =
    symbol "//"
        |. ignore zeroOrMore (not << isLineBreak)
        |> source
        |> map ((,) Comment >> List.singleton)


multilineComment : Parser (List Syntax)
multilineComment =
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


whitespace : Parser Syntax
whitespace =
    oneOf
        [ keep oneOrMore isSpace
            |> map ((,) Normal)
        , lineBreak
        ]


lineBreak : Parser Syntax
lineBreak =
    keep (Exactly 1) isLineBreak
        |> map ((,) LineBreak)


number : Parser Syntax
number =
    SyntaxHighlight.Helpers.number
        |> source
        |> map ((,) LiteralKeyword)


jsEscapable : Parser Syntax
jsEscapable =
    escapable
        |> source
        |> map ((,) LiteralKeyword)


end : List Syntax -> Parser (List Syntax)
end revSyntaxList =
    Parser.end
        |> map (always revSyntaxList)


toLines : List Syntax -> List Line
toLines revSyntaxList =
    List.foldl toLinesHelp ( [], [] ) revSyntaxList
        |> (\( lines, frags ) -> newLine frags :: lines)


toLinesHelp : Syntax -> ( List Line, List Fragment ) -> ( List Line, List Fragment )
toLinesHelp ( syntaxType, text ) ( lines, fragments ) =
    if syntaxType == LineBreak then
        ( newLine fragments :: lines
        , [ normal Default text ]
        )
    else
        ( lines, toFragment ( syntaxType, text ) :: fragments )


toFragment : Syntax -> Fragment
toFragment ( syntaxType, text ) =
    case syntaxType of
        Normal ->
            normal Default text

        Comment ->
            normal Color1 text

        String ->
            normal Color2 text

        Keyword ->
            normal Color3 text

        DeclarationKeyword ->
            emphasis Color4 text

        FunctionEval ->
            normal Color4 text

        Function ->
            normal Color5 text

        LiteralKeyword ->
            normal Color6 text

        Param ->
            normal Color7 text

        LineBreak ->
            normal Default text
