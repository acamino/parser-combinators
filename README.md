<a href="https://www.haskell.org">
  <img src="http://fieldstrength.org/images/haskell-logo.svg" alt="Haskell" align="right"  width="134" />
</a>

# Monadic Parser Combinators

[![Build Status](https://travis-ci.org/acamino/parser-combinators.svg?branch=master)](https://travis-ci.org/acamino/parser-combinators)

This repository is inspired in the chapter 8 of [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/book.html) book by Graham Hutton.

To check out the presentation please click [here](http://acamino.github.io/parser-combinators/#/).

## What is Covered?

### Parser Type

_A parser for things_  
_Is a function from strings_  
_To lists of pairs_  
_Of things and strings_

```haskell
type Parser a = String -> [(a, String)]
```

### Primitive Parsers
The **success** parser.

```haskell
return' :: a -> Parser a
return' v = \input -> [(v, input)]
```

The **failure** parser.

```haskell
failure :: Parser a
failure = \input -> []
```

The **item** parser.

```haskell
item :: Parser Char
item = \input -> case input of
                  []     -> []
                  (x:xs) -> [(x, xs)]
```
### Combinators

**Choice**. `+++` can be read as "or else".

```haskell
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case parse p input of
                     []         -> parse q input
                     [(v, out)] -> [(v, out)]
```

**Sequencing**. `>>>=` can be read as "then".

```haskell
(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \input -> case parse p input of
                      []         -> []
                      [(v, out)] -> parse (f v) out
```

### Derived Primitives

`sat`. Parse a character that _satisfies_ a predicate.

```haskell
sat :: (Char -> Bool) -> Parser Char
sat p = item >>>= \x ->
        if p x then return' x else failure
```

`digit`. Parse a character if it is a digit.

```haskell
digit :: Parser Char
digit = sat isDigit
```

Keep in mind that [`isDigit`](http://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Char.html#v:isDigit) is defined in `Data.Char`.

`char`. Parse a character if it is the given char.

```haskell
char :: Char -> Parser Char
char x = sat (==x)
```

`many`. Apply a parser zero or more times.  
`many1`. Apply a parser one or more times

```haskell
many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = p      >>>= \v  ->
          many p >>>= \vs ->
          return' (v:vs)
```

### Arithmetic Expressions
#### Grammar
```
ε means empty string
expr   = term (+ expr | ε)
term   = factor (* term | ε)
factor = (expr) | digit
digit  = 0 | 1 | 2 ...
```

So that `2 + 3 + 4` is:

<img src="http://acamino.github.io/parser-combinators/img/expression-tree.svg" alt="Expression Tree" width="40%" />


Then, the grammar is defined as follows:

1. `expr`

   ```haskell
   expr :: Parser Int
   expr = term >>>= \t ->
          (
            char '+' >>>= \_ ->
            expr     >>>= \e ->
            return' (t + e)
          )
          +++ return' t
   ```

1. `term`

   ```haskell
   term :: Parser Int
   term = factor >>>= \f ->
          (
            char '*' >>>= \_ ->
            term     >>>= \t ->
            return' (f * t)
          )
          +++ return' f
   ```

3. `factor`

   ```haskell
   factor :: Parser Int
   factor =
     (
       char '(' >>>= \_ ->
       expr     >>>= \e ->
       char ')' >>>= \_ ->
       return' e
     )
     +++ (
           digit >>>= \d ->
           return' (digitToInt d)
         )
   ```

`eval` evaluates a given expression.

```haskell
eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
```

## Licence

The code in this repository is licensed under the terms of the [MIT License](http://www.opensource.org/licenses/mit-license.html).
Please see the [LICENSE](LICENSE) file for details.
