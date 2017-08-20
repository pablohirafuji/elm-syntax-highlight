module SyntaxHighlight.Theme
    exposing
        ( monokai
        , github
        )

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
