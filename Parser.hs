module Parser (parseLambda, parseLine) where

import Data.Char (isDigit)
import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Data.Function
import Data.Ord


import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

failParser :: Parser a 
failParser = Parser $ \s -> Nothing

-- Functor instance
instance Functor Parser where 
  fmap f mp = 
    do 
      x <- mp
      return $ f x

-- Applicative instance
instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  af <*> mp = 
    do 
      f <- af
      v <- mp
      return $ f v

-- Monad instance
instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  mp >>= f = Parser $ 
    \s -> case parse mp s of 
            Nothing -> Nothing
            Just (val,rest) -> parse (f val) rest

-- Alternative instance
instance Alternative Parser where
  empty = failParser
  p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s
                                x -> x

-- Parsarea unui singur caracter
charParser :: Char -> Parser Char
charParser c = Parser $
  \s -> case s of 
           [] -> Nothing
           (x:xs) -> if x == c then Just (c,xs) else Nothing

-- Parsarea unui predicat de caractere
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $
  \s -> case s of 
           [] -> Nothing
           (x:xs) -> if p x then Just (x,xs) else Nothing

-- Parsarea unui identificator de variabilă
alpha :: Parser Char
alpha = predicateParser isAlpha

-- Parsarea unui șir
stringParser :: String -> Parser String
stringParser = traverse charParser

-- Parsarea unei variabile
varParser :: Parser Lambda
varParser = Var <$> some alpha

-- Parsarea unui șir de caractere
starParser :: (Parser a) -> Parser [a]
starParser p = many p

-- Ignorarea spațiilor
whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

-- Parsarea unei aplicații
application :: Parser Lambda
application = do
    charParser '('
    whitespaceParser
    e1 <- lambdaExpr
    whitespaceParser
    e2 <- lambdaExpr
    whitespaceParser
    charParser ')'
    return (App e1 e2)

-- Parsarea unei abstracții
abstraction :: Parser Lambda
abstraction = do
    charParser '\\'
    v <- some alpha
    charParser '.'
    e <- lambdaExpr
    return (Abs v e)

-- Parsarea unui macro
macro :: Parser Lambda
macro = do
    name <- some (predicateParser isUpper <|> predicateParser isDigit)
    return (Macro name)

-- Parsarea unei expresii lambda
lambdaExpr :: Parser Lambda
lambdaExpr = macro <|> abstraction <|> application <|> varParser

-- Funcția de parsare principală
parseLambda :: String -> Lambda
parseLambda input = 
  case parse (lambdaExpr <* end) input of
    Just (expr, "") -> expr
    _ -> error "Error"

-- Parsarea sfârșitului de intrare
end :: Parser ()
end = Parser $ 
  \input -> if null input
              then Just ((), "")
              else Nothing

-- Parsarea unei legături
binding :: Parser Line
binding = do
    name <- some alpha
    whitespaceParser
    charParser '='
    whitespaceParser
    expr <- lambdaExpr
    return (Binding name expr)

-- 3.3.
parseLine :: String -> Either String Line
parseLine input =
  case parse (lambdaExpr <* end) input of
    Just (expr, "") -> Right (Eval expr)
    Nothing         ->
      case parse (binding <* end) input of
        Just (binding, "") -> Right binding
        Nothing            -> Left "Error"