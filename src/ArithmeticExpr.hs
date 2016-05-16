module ArithmeticExpr where

import Data.Char (digitToInt)

import Parser ( (+++), char, parse, digit, Parser )

-- | Grammar for arithmetic expressions.
--
-- ε means empty string
-- expr   = term (+ expr | ε)
-- term   = factor (* term | ε)
-- factor = (expr) | digit
-- digit  = 0 | 1 | 2 ...

-- | Parser for expressions.
-- expr = term (+ expr | ε)

expr :: Parser Int
expr = do t <- term
          do char '+'
             e <- expr
             return (t + e)
           +++ return t

-- | Parser for terms.
-- term = factor (* term | ε)

term :: Parser Int
term = do f <- factor
          do char '*'
             t <- term
             return (f * t)
           +++ return f

-- | Parser for factors.
-- factor = (expr) | digit

factor :: Parser Int
factor = do char '('
            e <- expr
            char ')'
            return e
          +++ do d <- digit
                 return (digitToInt d)

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
