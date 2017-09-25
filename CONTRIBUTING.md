# Running the demo

```shell
cd demo
elm-reactor
```

Demo will be available at http://localhost:8000/index.html


# Creating a Theme

TODO

# Creating a Language

In the big picture, the process looks like this:

- Create a type for your language syntax.
- Make a parsing function that return a reverse list of `SyntaxHighlight.Language.Type.Token a`, with your created syntax type being the `a`.
  - The `Normal`, `LineBreak` and `Comment` types are already defined in `SyntaxHighlight.Language.Type.Syntax`. You must parse all line breaks and give each one the `LineBreak` syntax.
  - There is a bunch of helpers in `SyntaxHighlight.Language.Helpers`, but you are free to not use it.
- Make a `syntaxToStyle` function that take your created syntax type and return a tuple containing a `SyntaxHighlight.Style.Required` and a string unique for this syntax. The required style will be used when no specific style is defined for the syntax in the chosen theme.
- Use the `SyntaxHighlight.Line.Helpers.toLines` to transform your reversed tokens and `syntaxToStyle` function into a `List Line`.
- Expose this function in the file `SyntaxHighlight.elm`.
- Add the language in `SyntaxHighlight.Theme.Type.Syntax`.