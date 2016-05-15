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
