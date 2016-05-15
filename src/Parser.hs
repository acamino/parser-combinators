module Parser where

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
