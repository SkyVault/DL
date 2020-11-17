# Harp lang

## Thoughts

For the type system, I think it would be an interesting idea to instead of having types, rather use pattern matching to match each
parameter's structure, using a bnf like expression, for example:

Value = Number | String | Bool | Atom
List = Value : List | []

where ':' is the cons operator

## Syntax

The only has 3 types of syntax

```bnf
<expr> ::= <func-decl>
         | <let-expr>
         | <bin-op>
```

```



RULES:

a block if it is a single expression can start with a ':', a progn will be a 'do done' block

```
