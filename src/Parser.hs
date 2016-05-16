module Parser where

import Control.Applicative ( Applicative(..), Alternative(..) )
import Control.Monad       ( liftM, ap, MonadPlus(..) )
import Data.Char           ( isDigit )

infixr 5 +++

-- | A parser for things is s a function from strings
-- to lists of pairs of things and strings.

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

-- | The monad of parsers.

instance Monad Parser where
  return v = P (\input -> [(v, input)])
  p >>= f  = P (\input -> case parse p input of
                            []         -> []
                            [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
  mzero       = P (\input -> [])
  p `mplus` q = P (\input -> case parse p input of
                               []         -> parse q input
                               [(v, out)] -> [(v, out)])

failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\item -> case item of
                     []     -> []
                     (x:xs) -> [(x, xs)])


parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input


-- | The choice combinator can be read as "or else".

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (c==)

-- | many p and many1 p, apply a parser p as many times as possible until it
-- fails.

many' :: Parser a -> Parser [a]
many' p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many' p
             return (v:vs)

-- | Parser for lists.

p :: Parser String
p = do char '['
       x  <- digit
       xs <- many' (do char ','
                       digit)
       char ']'
       return (x:xs)
