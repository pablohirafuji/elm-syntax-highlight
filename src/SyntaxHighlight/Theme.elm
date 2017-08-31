module SyntaxHighlight.Theme
    exposing
        ( all
        , monokai
        , github
        , oneDark
        )

-- Add all themes name and code here to show in the Themes page


all : List ( String, String )
all =
    [ ( "Monokai", monokai )
    , ( "GitHub", github )
    , ( "One Dark", oneDark )
    ]



-- Monokai inspired theme


monokai : String
monokai =
    """
.elmsh {
    background: #23241f;
    color: #f8f8f2;
}

.elmsh-hl {
    background: #0e0f0d;
}

.elmsh-add {
    background: #003800;
}

.elmsh-del {
    background: #380000;
}

.elmsh-strong {
    font-weight: bold;
}

.elmsh-emphasis {
    font-style: italic;
}

.elmsh1 {
    color: #75715e;
}
.elmsh2 {
    color: #e6db74;
}

.elmsh3 {
    color: #f92672;
}

.elmsh4 {
    color: #66d9ef;
}

.elmsh5 {
    color: #a6e22e;
}

.elmsh6 {
    color: #ae81ff;
}

.elmsh7 {
    color: #fd971f;
}"""



-- GitHub inspired theme


github : String
github =
    """
.elmsh {
    background: white;
    color: #24292e;
}

.elmsh-hl {
    background: #fffbdd;
}

.elmsh-add {
    background: #eaffea;
}

.elmsh-del {
    background: #ffecec;
}

.elmsh-strong {
    font-weight: bold;
}

.elmsh-emphasis {
    font-style: italic;
}

.elmsh1 {
    color: #969896;
}
.elmsh2 {
    color: #df5000;
}

.elmsh3 {
    color: #d73a49;
}

.elmsh4 {
    color: #0086b3;
}

.elmsh5 {
    color: #63a35c;
}

.elmsh6 {
    color: #005cc5;
}

.elmsh7 {
    color: #795da3;
}"""



{-
   Atom One Dark inspired theme
   https://github.com/atom/one-dark-syntax

   base:    #282c34
   mono-1:  #abb2bf
   mono-2:  #818896
   mono-3:  #5c6370
   hue-1:   #56b6c2
   hue-2:   #61aeee
   hue-3:   #c678dd
   hue-4:   #98c379
   hue-5:   #e06c75
   hue-5-2: #be5046
   hue-6:   #d19a66
   hue-6-2: #e6c07b
-}


oneDark : String
oneDark =
    """
.elmsh {
  background: #282c34;
  color: #abb2bf;
}

.elmsh-hl {
  background: rgba(229,231,235, 0.1);
}

.elmsh-add {
  background: rgba(40,124,82, 0.4);
}

.elmsh-del {
  background: rgba(136,64,67, 0.4);
}

.elmsh-strong {
  font-weight: bold;
}

.elmsh-emphasis {
  font-style: italic;
}

.elmsh1 {
  color: #5c6370;
  font-style: italic
}
.elmsh2 {
  color: #98c379;
}

.elmsh3 {
  color: #c678dd;
}

.elmsh4 {
  color: #c678dd;
}

.elmsh5 {
  color: #61aeee;
}

.elmsh6 {
  color: #c678dd;
}

.elmsh7 {
  color: #abb2bf;
}"""
