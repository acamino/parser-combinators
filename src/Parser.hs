module Parser where

import Data.Char (isDigit)

-- | A parser for things is s a function from strings
-- to lists of pairs of things and strings.

type Parser a = String -> [(a, String)]

return' :: a -> Parser a
return' v = \input -> [(v, input)]

failure :: Parser a
failure = \input -> []

item :: Parser Char
item = \item -> case item of
                  []     -> []
                  (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p input = p input

-- | The choice combinator can be read as "or else".

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case parse p input of
                      []         -> parse q input
                      [(v, out)] -> [(v, out)]

-- | The sequencing combinator can be read as "then".

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \input -> case parse p input of
                       []         -> []
                       [(v, out)] -> parse (f v) out

sat :: (Char -> Bool) -> Parser Char
sat p = item >>>= \c ->
        if p c then return' c else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (c==)

-- | many p and many1 p, apply a parser p as many times as possible until it
-- fails.

many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = p      >>>= \v  ->
          many p >>>= \vs ->
          return' (v:vs)
