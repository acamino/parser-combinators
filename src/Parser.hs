module Parser
  ( Parser
  , (+++)
  , (>>=)
  , char
  , digit
  , failure
  , item
  , lists
  , many
  , many1
  , parse
  , return
  , sat
  )

 where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Data.Char           (isDigit)

infixr 5 +++


-- | A parser for things is s a function from strings
-- to lists of pairs of things and strings.

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero


-- | The Monad instance for parsers.

instance Monad Parser where
  return v = Parser (\input -> [(v, input)])
  p >>= f  = Parser (\input ->
    case parse p input of
      []         -> []
      [(v, out)] -> parse (f v) out)
  p >> f   = p >>= \_ -> f

instance MonadPlus Parser where
  mzero       = Parser (const [])
  p `mplus` q = Parser (\input ->
    case parse p input of
      []         -> parse q input
      [(v, out)] -> [(v, out)])


failure :: Parser a
failure = mzero


item :: Parser Char
item = Parser (\input ->
  case input of
    []     -> []
    (x:xs) -> [(x, xs)])


parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p


-- | The choice combinator can be read as "or else".

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
     then return c
     else failure

digit :: Parser Char
digit = sat isDigit


char :: Char -> Parser Char
char c = sat (c ==)


-- | many p and many1 p, apply a parser p as many times as possible until it
-- fails.

many :: Parser a -> Parser [a]
many p = many1 p +++ return []


many1 :: Parser a -> Parser [a]
many1 p = do
  v  <- p
  vs <- many p
  return (v:vs)


-- | Parser for lists.

lists :: Parser String
lists = do
  void $ char '['
  x  <- digit
  xs <- many $ char ',' >> digit
  void $ char ']'
  return (x:xs)
