module ArithmeticExpr where

import Data.Char (digitToInt)
import Parser ((>>>=), (+++), char, return', parse, digit, Parser)

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
expr = term >>>= \t ->
       (
         char '+' >>>= \_ ->
         expr     >>>= \e ->
         return' (t + e)
       )
       +++ return' t

-- | Parser for terms.
-- term = factor (* term | ε)

term :: Parser Int
term = factor >>>= \f ->
       (
         char '*' >>>= \_ ->
         term     >>>= \t ->
         return' (f * t)
       )
       +++ return' f

-- | Parser for factors.
-- factor = (expr) | digit

factor :: Parser Int
factor = (
           char '(' >>>= \_ ->
           expr     >>>= \e ->
           char ')' >>>= \_ ->
           return' e
         )
         +++ (
               digit >>>= \d ->
               return' (digitToInt d)
             )

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
