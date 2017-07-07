<img src="https://dl.dropboxusercontent.com/s/1ihq4owuftighi3/parsers-combinators.png" alt="Parser Combinators" />

# Monadic Parser Combinators
[![Build Status](https://travis-ci.org/acamino/parser-combinators.svg?branch=master)](https://travis-ci.org/acamino/parser-combinators)

This repository contains the support material for a presentation I made about
"monadic parser combinators". I took the key concepts from the chapter 8 of the
book [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/book.html) book
by Graham Hutton.

If you want to check out the slides of the aforementioned presentation, plase
click [here](http://acamino.github.io/parser-combinators/#/).

## What is Covered?

### Parser Type

> _A parser for things_  
> _Is a function from strings_  
> _To lists of pairs_  
> _Of things and strings_

```haskell
type Parser a = String -> [(a, String)]
```

### Primitive Parsers
The **success** parser.

```haskell
return :: a -> Parser a
return v = \input -> [(v, input)]
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

**Sequencing**. `>>=` can be read as "then" or even better, "bind".

```haskell
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \input -> case parse p input of
                      []         -> []
                      [(v, out)] -> parse (f v) out
```

### Derived Primitives

`sat`. Parse a character that _satisfies_ a predicate.

```haskell
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
     then return c
     else failure
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
char c = sat (c ==)
```

`many`. Apply a parser zero or more times.  
`many1`. Apply a parser one or more times

```haskell
many' :: Parser a -> Parser [a]
many' p = many1 p +++ return []


many1 :: Parser a -> Parser [a]
many1 p = do
  v  <- p
  vs <- many' p
  return (v:vs)
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
   expr = do
     t <- term
     addExprTo t +++ return t
     where
       addExprTo t = do
         char '+'
         e <- expr
         return (t + e)
   ```

1. `term`

   ```haskell
   term :: Parser Int
   term = do
     f <- factor
     multTermWith f +++ return f
     where
       multTermWith f = do
         char '*'
         t <- term
         return (f * t)
   ```

3. `factor`

   ```haskell
   factor :: Parser Int
   factor = do
     expr' +++ digit'
       where
         expr' = do
           char '('
           e <- expr
           char ')'
           return e
         digit' = do
           d <- digit
           return (digitToInt d)
   ```

`eval` evaluates a given expression.

```haskell
eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
```

## Local Development

1. Fork the project [on GitHub](https://github.com/acamino/parser-combinators)
   and clone your fork locally.

   ```bash
   $ git clone git://github.com/username/parser-combinators.git
   $ cd parser-combinators
   $ git remote add upstream https://github.com/acamino/parser-combinators.git
   ```

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/).

1. Get the appropriate GHC for the project.

   ```bash
   $ stack setup
   ```

1. Make sure the tests succeed.

   ```bash
   $ stack test
   ```

1. If you want to launch a REPL and have fun with this parser.

   ```bash
   $ stack repl
   ```

## Issues & Support

Please [file tickets](https://github.com/acamino/parser-combinators/issues) for
bug or problems.

## Contributing

Edits and enhancements are welcome. Just fork the repository, make your changes
and send me a pull request.

## Licence

The code in this repository is licensed under the terms of the
[MIT License](http://www.opensource.org/licenses/mit-license.html).  
Please see the [LICENSE](LICENSE) file for details.
