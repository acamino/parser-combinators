-- | Grammar for arithmetic expressions.
--
-- ε means empty string
-- expr   = term (+ expr | ε)
-- term   = factor (* term | ε)
-- factor = (expr) | digit
-- digit  = 0 | 1 | 2 ...

module ArithmeticExpr
  ( eval
  )
where

import Data.Char (digitToInt)
import Parser    (Parser, char, parse, digit, (+++))


-- | Parser for expressions.
-- expr = term (+ expr | ε)

expr :: Parser Int
expr = do
  t <- term
  addExprTo t +++ return t
  where
    addExprTo t = do
      char '+'
      e <- expr
      return (t + e)


-- | Parser for terms.
-- term = factor (* term | ε)

term :: Parser Int
term = do
  f <- factor
  multTermWith f +++ return f
  where
    multTermWith f = do
      char '*'
      t <- term
      return (f * t)


-- | Parser for factors.
-- factor = (expr) | digit

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


-- | Simple arithmetic expressions evaluator.

eval :: String -> Int
eval xs = case parse expr xs of
            [(n, [])]  -> n
            [(_, out)] -> error ("unused input " ++ out)
            []         -> error "invalid input"
