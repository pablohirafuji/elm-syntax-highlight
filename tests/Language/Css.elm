module Language.Css exposing (suite)

import Expect exposing (Expectation, equal, onFail)
import Fuzz exposing (string)
import Parser
import Result exposing (Result(..))
import SyntaxHighlight.Language.Css as Css exposing (..)
import SyntaxHighlight.Language.Type as T exposing (Syntax(..))
import Test exposing (..)


suite : Test
suite =
    describe "Css Language Test Suite"
        [ equalTest "Namespace at-rule" namespaceStr namespaceResult
        , equalTest "Import at-rule" importStr importResult
        , equalTest "Media Query at-rule" mediaQueryStr mediaQueryResult
        , equalTest "Font Face at-rule" fontFaceStr fontFaceResult
        , equalTest "Keyframes at-rule" keyframesStr keyframesResult
        , equalTest "Counter Style at-rule" counterStyleStr counterStyleResult
        , equalTest "Page at-rule" pageStr pageResult
        , equalTest "Font Feature Values at-rule" fontFeatureValuesStr fontFeatureValuesResult
        , equalTest "Charset at-rule" charSetStr charSetResult
        , fuzz string "Fuzz string" <|
            \fuzzStr ->
                Parser.run Css.toRevTokens fuzzStr
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok fuzzStr)
                    |> onFail ("Resulting error string: \"" ++ fuzzStr ++ "\"")
        ]


type alias ParserResult =
    Result (List Parser.DeadEnd) (List ( T.Syntax Css.Syntax, String ))


equalTest : String -> String -> ParserResult -> Test
equalTest title strToTest expected =
    describe title
        [ test "Syntax equality" <|
            \() ->
                Parser.run Css.toRevTokens strToTest
                    |> Result.map List.reverse
                    |> equal expected
        , test "String equality" <|
            \() ->
                Parser.run Css.toRevTokens strToTest
                    |> Result.map
                        (List.reverse
                            >> List.map Tuple.second
                            >> String.concat
                        )
                    |> equal (Ok strToTest)
        ]



-- Codes from https://developer.mozilla.org/en-US/docs/Web/CSS


namespaceStr : String
namespaceStr =
    """@namespace prefix url(XML-namespace-URL);
@namespace prefix "XML-namespace-URL";
@namespace url(http://www.w3.org/1999/xhtml);
@namespace svg "http://www.w3.org/2000/svg";"""


namespaceResult : ParserResult
namespaceResult =
    Ok [ ( C (AtRule Identifier), "@namespace" ), ( Normal, " " ), ( C (AtRule Prefix), "prefix" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "XML-namespace-URL" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@namespace" ), ( Normal, " " ), ( C (AtRule Prefix), "prefix" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "XML-namespace-URL" ), ( C String, "\"" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@namespace" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "http://www.w3.org/1999/xhtml" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@namespace" ), ( Normal, " " ), ( C (AtRule Prefix), "svg" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "http://www.w3.org/2000/svg" ), ( C String, "\"" ), ( Normal, ";" ) ]


importStr : String
importStr =
    """@import url("fineprint.css") print;
@import url("bluish.css") projection, tv;
@import 'custom.css';
@import url("chrome://communicator/skin/");
@import "common.css" screen, projection;
@import url('landscape.css') screen and (orientation:landscape);"""


importResult : ParserResult
importResult =
    Ok [ ( C (AtRule Identifier), "@import" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "\"" ), ( C String, "fineprint.css" ), ( C String, "\"" ), ( Normal, ")" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "print" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@import" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "\"" ), ( C String, "bluish.css" ), ( C String, "\"" ), ( Normal, ")" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "projection" ), ( Normal, "," ), ( Normal, " " ), ( C (AtRule AtRuleValue), "tv" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@import" ), ( Normal, " " ), ( C String, "'" ), ( C String, "custom.css" ), ( C String, "'" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@import" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "\"" ), ( C String, "chrome://communicator/skin/" ), ( C String, "\"" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@import" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "common.css" ), ( C String, "\"" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "screen" ), ( Normal, "," ), ( Normal, " " ), ( C (AtRule AtRuleValue), "projection" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@import" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "'" ), ( C String, "landscape.css" ), ( C String, "'" ), ( Normal, ")" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "screen" ), ( Normal, " " ), ( C (AtRule Keyword), "and" ), ( Normal, " " ), ( Normal, "(" ), ( C (AtRule AtRuleValue), "orientation" ), ( Normal, ":" ), ( C (AtRule AtRuleValue), "landscape" ), ( Normal, ")" ), ( Normal, ";" ) ]


mediaQueryStr : String
mediaQueryStr =
    """@media screen and (min-width: 900px) {
    article {
        padding: 1rem 3rem;
    }
}

@supports (display: flex) {
    @media screen and (min-width: 900px) {
        article {
            display: flex;
        }
    }
}"""


mediaQueryResult : ParserResult
mediaQueryResult =
    Ok [ ( C (AtRule Identifier), "@media" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "screen" ), ( Normal, " " ), ( C (AtRule Keyword), "and" ), ( Normal, " " ), ( Normal, "(" ), ( C (AtRule AtRuleValue), "min-width" ), ( Normal, ":" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "900px" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (Selector Element), "article" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C Property, "padding" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "1" ), ( C Unit, "rem" ), ( Normal, " " ), ( C Number, "3" ), ( C Unit, "rem" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@supports" ), ( Normal, " " ), ( Normal, "(" ), ( C (AtRule AtRuleValue), "display" ), ( Normal, ":" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "flex" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (AtRule Identifier), "@media" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "screen" ), ( Normal, " " ), ( C (AtRule Keyword), "and" ), ( Normal, " " ), ( Normal, "(" ), ( C (AtRule AtRuleValue), "min-width" ), ( Normal, ":" ), ( Normal, " " ), ( C (AtRule AtRuleValue), "900px" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C (Selector Element), "article" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "            " ), ( C Property, "display" ), ( Normal, ":" ), ( Normal, " " ), ( C PropertyValue, "flex" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]


fontFaceStr : String
fontFaceStr =
    """@font-face {
    font-family: MyHelvetica;
    src: local("Helvetica Neue Bold"),
        local("HelveticaNeue-Bold"),
        url(MgOpenModernaBold.ttf);
    font-weight: bold;
}
"""


fontFaceResult : ParserResult
fontFaceResult =
    Ok [ ( C (AtRule Identifier), "@font-face" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "font-family" ), ( Normal, ":" ), ( Normal, " " ), ( C PropertyValue, "MyHelvetica" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "src" ), ( Normal, ":" ), ( Normal, " " ), ( C PropertyValue, "local" ), ( Normal, "(" ), ( C String, "\"" ), ( C String, "Helvetica Neue Bold" ), ( C String, "\"" ), ( Normal, ")," ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C PropertyValue, "local" ), ( Normal, "(" ), ( C String, "\"" ), ( C String, "HelveticaNeue-Bold" ), ( C String, "\"" ), ( Normal, ")," ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "MgOpenModernaBold.ttf" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "font-weight" ), ( Normal, ":" ), ( Normal, " " ), ( C PropertyValue, "bold" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ) ]


keyframesStr : String
keyframesStr =
    """@keyframes identifier {
    0% { top: 0; }
    50% { top: 30px; left: 20px; }
    50% { top: 10px; }
    100% { top: 0; }
}"""


keyframesResult : ParserResult
keyframesResult =
    Ok [ ( C (AtRule Identifier), "@keyframes" ), ( Normal, " " ), ( C (AtRule Prefix), "identifier" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (Selector Element), "0%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( C Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "0" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (Selector Element), "50%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( C Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "30" ), ( C Unit, "px" ), ( Normal, ";" ), ( Normal, " " ), ( C Property, "left" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "20" ), ( C Unit, "px" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (Selector Element), "50%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( C Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "10" ), ( C Unit, "px" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (Selector Element), "100%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( C Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "0" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]


counterStyleStr : String
counterStyleStr =
    """@counter-style winners-list {
    system: fixed;
    symbols: url(gold-medal.svg) url(silver-medal.svg) url(bronze-medal.svg);
    suffix: " ";
}
"""


counterStyleResult : ParserResult
counterStyleResult =
    Ok [ ( C (AtRule Identifier), "@counter-style" ), ( Normal, " " ), ( C (AtRule Prefix), "winners-list" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "system" ), ( Normal, ":" ), ( Normal, " " ), ( C PropertyValue, "fixed" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "symbols" ), ( Normal, ":" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "gold-medal.svg" ), ( Normal, ")" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "silver-medal.svg" ), ( Normal, ")" ), ( Normal, " " ), ( C PropertyValue, "url" ), ( Normal, "(" ), ( C String, "bronze-medal.svg" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "suffix" ), ( Normal, ":" ), ( Normal, " " ), ( C String, "\"" ), ( C String, " " ), ( C String, "\"" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ) ]


pageStr : String
pageStr =
    """@page {
    margin: 1cm;
}

@page :first {
    margin: 2cm;
}"""


pageResult : ParserResult
pageResult =
    Ok [ ( C (AtRule Identifier), "@page" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "margin" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "1" ), ( C Unit, "cm" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@page" ), ( Normal, " " ), ( C (Selector PseudoClass), ":first" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C Property, "margin" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "2" ), ( C Unit, "cm" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]


fontFeatureValuesStr : String
fontFeatureValuesStr =
    """@font-feature-values Font One {
    @styleset {
      nice-style: 12;
    }
}

@font-feature-values Font Two {
    @styleset {
        nice-style: 4;
    }
}
"""


fontFeatureValuesResult : ParserResult
fontFeatureValuesResult =
    Ok [ ( C (AtRule Identifier), "@font-feature-values" ), ( Normal, " " ), ( C (AtRule Prefix), "Font" ), ( Normal, " " ), ( C (AtRule Prefix), "One" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (AtRule Identifier), "@styleset" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "      " ), ( C Property, "nice-style" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "12" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@font-feature-values" ), ( Normal, " " ), ( C (AtRule Prefix), "Font" ), ( Normal, " " ), ( C (AtRule Prefix), "Two" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( C (AtRule Identifier), "@styleset" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "        " ), ( C Property, "nice-style" ), ( Normal, ":" ), ( Normal, " " ), ( C Number, "4" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ) ]


charSetStr : String
charSetStr =
    """@charset "UTF-8";       /* Set the encoding of the style sheet to Unicode UTF-8 */
@charset 'iso-8859-15'; /* Invalid, wrong quoting style used */
@charset  "UTF-8";      /* Invalid, more than one space */
@charset "UTF-8";      /* Invalid, there is a character (a space) before the at-rule */
@charset UTF-8;         /* Invalid, without ' or ", the charset is not a CSS <string> */
"""


charSetResult : ParserResult
charSetResult =
    Ok [ ( C (AtRule Identifier), "@charset" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "UTF-8" ), ( C String, "\"" ), ( Normal, ";" ), ( Normal, "       " ), ( Comment, "/*" ), ( Comment, " Set the encoding of the style sheet to Unicode UTF-8 " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@charset" ), ( Normal, " " ), ( C String, "'" ), ( C String, "iso-8859-15" ), ( C String, "'" ), ( Normal, ";" ), ( Normal, " " ), ( Comment, "/*" ), ( Comment, " Invalid, wrong quoting style used " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@charset" ), ( Normal, "  " ), ( C String, "\"" ), ( C String, "UTF-8" ), ( C String, "\"" ), ( Normal, ";" ), ( Normal, "      " ), ( Comment, "/*" ), ( Comment, " Invalid, more than one space " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@charset" ), ( Normal, " " ), ( C String, "\"" ), ( C String, "UTF-8" ), ( C String, "\"" ), ( Normal, ";" ), ( Normal, "      " ), ( Comment, "/*" ), ( Comment, " Invalid, there is a character (a space) before the at-rule " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( C (AtRule Identifier), "@charset" ), ( Normal, " " ), ( C String, "UTF-8" ), ( Normal, ";" ), ( Normal, "         " ), ( Comment, "/*" ), ( Comment, " Invalid, without ' or \", the charset is not a CSS <string> " ), ( Comment, "*/" ), ( LineBreak, "\n" ) ]
