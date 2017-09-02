module Language.Css exposing (suite)

import Result exposing (Result(..))
import Expect exposing (Expectation, equal)
import Test exposing (..)
import Parser
import SyntaxHighlight.Language.Css as Css exposing (..)


suite : Test
suite =
    describe "Css Language Test Suite"
        [ test "Namespace at-rule" <|
            \() ->
                Css.toSyntax namespaceStr
                    |> Result.map List.reverse
                    |> equal namespaceResult
        , test "Import at-rule" <|
            \() ->
                Css.toSyntax importStr
                    |> Result.map List.reverse
                    |> equal importResult
        , test "Media Query at-rule" <|
            \() ->
                Css.toSyntax mediaQueryStr
                    |> Result.map List.reverse
                    |> equal mediaQueryResult
        , test "Font Face at-rule" <|
            \() ->
                Css.toSyntax fontFaceStr
                    |> Result.map List.reverse
                    |> equal fontFaceResult
        , test "Keyframes at-rule" <|
            \() ->
                Css.toSyntax keyframesStr
                    |> Result.map List.reverse
                    |> equal keyframesResult
        , test "Counter Style at-rule" <|
            \() ->
                Css.toSyntax counterStyleStr
                    |> Result.map List.reverse
                    |> equal counterStyleResult
        , test "Page at-rule" <|
            \() ->
                Css.toSyntax pageStr
                    |> Result.map List.reverse
                    |> equal pageResult
        , test "Font Feature Values at-rule" <|
            \() ->
                Css.toSyntax fontFeatureValuesStr
                    |> Result.map List.reverse
                    |> equal fontFeatureValuesResult
        , test "Charset at-rule" <|
            \() ->
                Css.toSyntax charSetStr
                    |> Result.map List.reverse
                    |> equal charSetResult
        ]



-- Codes from https://developer.mozilla.org/en-US/docs/Web/CSS


namespaceStr : String
namespaceStr =
    """@namespace prefix url(XML-namespace-URL);
@namespace prefix "XML-namespace-URL";
@namespace url(http://www.w3.org/1999/xhtml);
@namespace svg "http://www.w3.org/2000/svg";"""


namespaceResult : Result Parser.Error (List ( SyntaxType, String ))
namespaceResult =
    Ok [ ( AtRule Identifier, "@namespace" ), ( Normal, " " ), ( AtRule Prefix, "prefix" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "XML-namespace-URL" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@namespace" ), ( Normal, " " ), ( AtRule Prefix, "prefix" ), ( Normal, " " ), ( String, "\"" ), ( String, "XML-namespace-URL" ), ( String, "\"" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@namespace" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "http://www.w3.org/1999/xhtml" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@namespace" ), ( Normal, " " ), ( AtRule Prefix, "svg" ), ( Normal, " " ), ( String, "\"" ), ( String, "http://www.w3.org/2000/svg" ), ( String, "\"" ), ( Normal, ";" ) ]


importStr : String
importStr =
    """@import url("fineprint.css") print;
@import url("bluish.css") projection, tv;
@import 'custom.css';
@import url("chrome://communicator/skin/");
@import "common.css" screen, projection;
@import url('landscape.css') screen and (orientation:landscape);"""


importResult : Result Parser.Error (List ( SyntaxType, String ))
importResult =
    Ok [ ( AtRule Identifier, "@import" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "\"" ), ( String, "fineprint.css" ), ( String, "\"" ), ( Normal, ")" ), ( Normal, " " ), ( AtRule AtRuleValue, "print" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@import" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "\"" ), ( String, "bluish.css" ), ( String, "\"" ), ( Normal, ")" ), ( Normal, " " ), ( AtRule AtRuleValue, "projection" ), ( Normal, "," ), ( Normal, " " ), ( AtRule AtRuleValue, "tv" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@import" ), ( Normal, " " ), ( String, "'" ), ( String, "custom.css" ), ( String, "'" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@import" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "\"" ), ( String, "chrome://communicator/skin/" ), ( String, "\"" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@import" ), ( Normal, " " ), ( String, "\"" ), ( String, "common.css" ), ( String, "\"" ), ( Normal, " " ), ( AtRule AtRuleValue, "screen" ), ( Normal, "," ), ( Normal, " " ), ( AtRule AtRuleValue, "projection" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@import" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "'" ), ( String, "landscape.css" ), ( String, "'" ), ( Normal, ")" ), ( Normal, " " ), ( AtRule AtRuleValue, "screen" ), ( Normal, " " ), ( AtRule Keyword, "and" ), ( Normal, " " ), ( Normal, "(" ), ( AtRule AtRuleValue, "orientation" ), ( Normal, ":" ), ( AtRule AtRuleValue, "landscape" ), ( Normal, ")" ), ( Normal, ";" ) ]


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


mediaQueryResult : Result Parser.Error (List ( SyntaxType, String ))
mediaQueryResult =
    Ok [ ( AtRule Identifier, "@media" ), ( Normal, " " ), ( AtRule AtRuleValue, "screen" ), ( Normal, " " ), ( AtRule Keyword, "and" ), ( Normal, " " ), ( Normal, "(" ), ( AtRule AtRuleValue, "min-width" ), ( Normal, ":" ), ( Normal, " " ), ( AtRule AtRuleValue, "900px" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Selector Element, "article" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Property, "padding" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "1" ), ( Unit, "rem" ), ( Normal, " " ), ( Number, "3" ), ( Unit, "rem" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@supports" ), ( Normal, " " ), ( Normal, "(" ), ( AtRule AtRuleValue, "display" ), ( Normal, ":" ), ( Normal, " " ), ( AtRule AtRuleValue, "flex" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( AtRule Identifier, "@media" ), ( Normal, " " ), ( AtRule AtRuleValue, "screen" ), ( Normal, " " ), ( AtRule Keyword, "and" ), ( Normal, " " ), ( Normal, "(" ), ( AtRule AtRuleValue, "min-width" ), ( Normal, ":" ), ( Normal, " " ), ( AtRule AtRuleValue, "900px" ), ( Normal, ")" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Selector Element, "article" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "      " ), ( Property, "display" ), ( Normal, ":" ), ( Normal, " " ), ( PropertyValue, "flex" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]


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


fontFaceResult : Result Parser.Error (List ( SyntaxType, String ))
fontFaceResult =
    Ok [ ( AtRule Identifier, "@font-face" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "font-family" ), ( Normal, ":" ), ( Normal, " " ), ( PropertyValue, "MyHelvetica" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "src" ), ( Normal, ":" ), ( Normal, " " ), ( PropertyValue, "local" ), ( Normal, "(" ), ( String, "\"" ), ( String, "Helvetica Neue Bold" ), ( String, "\"" ), ( Normal, ")," ), ( LineBreak, "\n" ), ( Normal, "       " ), ( PropertyValue, "local" ), ( Normal, "(" ), ( String, "\"" ), ( String, "HelveticaNeue-Bold" ), ( String, "\"" ), ( Normal, ")," ), ( LineBreak, "\n" ), ( Normal, "       " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "MgOpenModernaBold.ttf" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "font-weight" ), ( Normal, ":" ), ( Normal, " " ), ( PropertyValue, "bold" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ) ]


keyframesStr : String
keyframesStr =
    """@keyframes identifier {
  0% { top: 0; }
  50% { top: 30px; left: 20px; }
  50% { top: 10px; }
  100% { top: 0; }
}"""


keyframesResult : Result Parser.Error (List ( SyntaxType, String ))
keyframesResult =
    Ok [ ( AtRule Identifier, "@keyframes" ), ( Normal, " " ), ( AtRule Prefix, "identifier" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Selector Element, "0%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "0" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Selector Element, "50%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "30" ), ( Unit, "px" ), ( Normal, ";" ), ( Normal, " " ), ( Property, "left" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "20" ), ( Unit, "px" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Selector Element, "50%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "10" ), ( Unit, "px" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Selector Element, "100%" ), ( Normal, " " ), ( Normal, "{" ), ( Normal, " " ), ( Property, "top" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "0" ), ( Normal, ";" ), ( Normal, " " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]


counterStyleStr : String
counterStyleStr =
    """@counter-style winners-list {
  system: fixed;
  symbols: url(gold-medal.svg) url(silver-medal.svg) url(bronze-medal.svg);
  suffix: " ";
}
"""


counterStyleResult : Result Parser.Error (List ( SyntaxType, String ))
counterStyleResult =
    Ok [ ( AtRule Identifier, "@counter-style" ), ( Normal, " " ), ( AtRule Prefix, "winners-list" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "system" ), ( Normal, ":" ), ( Normal, " " ), ( PropertyValue, "fixed" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "symbols" ), ( Normal, ":" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "gold-medal.svg" ), ( Normal, ")" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "silver-medal.svg" ), ( Normal, ")" ), ( Normal, " " ), ( PropertyValue, "url" ), ( Normal, "(" ), ( String, "bronze-medal.svg" ), ( Normal, ")" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "suffix" ), ( Normal, ":" ), ( Normal, " " ), ( String, "\"" ), ( String, " " ), ( String, "\"" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ) ]


pageStr : String
pageStr =
    """@page {
  margin: 1cm;
}

@page :first {
  margin: 2cm;
}"""


pageResult : Result Parser.Error (List ( SyntaxType, String ))
pageResult =
    Ok [ ( AtRule Identifier, "@page" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "margin" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "1" ), ( Unit, "cm" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@page" ), ( Normal, " " ), ( Selector PseudoClass, ":first" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Property, "margin" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "2" ), ( Unit, "cm" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "}" ) ]


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


fontFeatureValuesResult : Result Parser.Error (List ( SyntaxType, String ))
fontFeatureValuesResult =
    Ok [ ( AtRule Identifier, "@font-feature-values" ), ( Normal, " " ), ( AtRule Prefix, "Font" ), ( Normal, " " ), ( AtRule Prefix, "One" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( AtRule Identifier, "@styleset" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Property, "nice-style" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "12" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@font-feature-values" ), ( Normal, " " ), ( AtRule Prefix, "Font" ), ( Normal, " " ), ( AtRule Prefix, "Two" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( AtRule Identifier, "@styleset" ), ( Normal, " " ), ( Normal, "{" ), ( LineBreak, "\n" ), ( Normal, "    " ), ( Property, "nice-style" ), ( Normal, ":" ), ( Normal, " " ), ( Number, "4" ), ( Normal, ";" ), ( LineBreak, "\n" ), ( Normal, "  " ), ( Normal, "}" ), ( LineBreak, "\n" ), ( Normal, "}" ), ( LineBreak, "\n" ) ]


charSetStr : String
charSetStr =
    """@charset "UTF-8";       /* Set the encoding of the style sheet to Unicode UTF-8 */
@charset 'iso-8859-15'; /* Invalid, wrong quoting style used */
@charset  "UTF-8";      /* Invalid, more than one space */
 @charset "UTF-8";      /* Invalid, there is a character (a space) before the at-rule */
@charset UTF-8;         /* Invalid, without ' or ", the charset is not a CSS <string> */
"""


charSetResult : Result Parser.Error (List ( SyntaxType, String ))
charSetResult =
    Ok [ ( AtRule Identifier, "@charset" ), ( Normal, " " ), ( String, "\"" ), ( String, "UTF-8" ), ( String, "\"" ), ( Normal, ";" ), ( Normal, "       " ), ( Comment, "/*" ), ( Comment, " Set the encoding of the style sheet to Unicode UTF-8 " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@charset" ), ( Normal, " " ), ( String, "'" ), ( String, "iso-8859-15" ), ( String, "'" ), ( Normal, ";" ), ( Normal, " " ), ( Comment, "/*" ), ( Comment, " Invalid, wrong quoting style used " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@charset" ), ( Normal, "  " ), ( String, "\"" ), ( String, "UTF-8" ), ( String, "\"" ), ( Normal, ";" ), ( Normal, "      " ), ( Comment, "/*" ), ( Comment, " Invalid, more than one space " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( Normal, " " ), ( AtRule Identifier, "@charset" ), ( Normal, " " ), ( String, "\"" ), ( String, "UTF-8" ), ( String, "\"" ), ( Normal, ";" ), ( Normal, "      " ), ( Comment, "/*" ), ( Comment, " Invalid, there is a character (a space) before the at-rule " ), ( Comment, "*/" ), ( LineBreak, "\n" ), ( AtRule Identifier, "@charset" ), ( Normal, " " ), ( String, "UTF-8" ), ( Normal, ";" ), ( Normal, "         " ), ( Comment, "/*" ), ( Comment, " Invalid, without ' or \", the charset is not a CSS <string> " ), ( Comment, "*/" ), ( LineBreak, "\n" ) ]
