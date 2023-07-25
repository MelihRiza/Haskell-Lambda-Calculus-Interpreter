module Parser (parse_expr, parse_code) where

import Control.Monad
import Control.Applicative
import Expr
import Data.Char

-- Parser data type
newtype Parser a = Parser {
    parse :: String -> Maybe (a, String)
}

--- type declaration ---

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    (Parser p) >>= f = Parser $ \s -> case p s of
        Just (x, s') -> parse (f x) s'
        _ -> Nothing

instance Applicative Parser where
    pure x = return x
    pf <*> px = do
        f <- pf
        x <- px
        return $ f x

instance Functor Parser where
    fmap f px = do
        x <- px
        return $ f x

instance Alternative Parser where
  empty = failParser
  (Parser p1) <|> (Parser p2) = Parser $ \x -> p1 x <|> p2 x 


--- type declaration over ---

-- TODO 2.1. parse an expression
parse_expr :: String -> Expr
parse_expr inputString = 
  case parse exprParser inputString of
    Just (expr, "") -> expr
    _ -> error "EROOR: could not parse expr"


failParser :: Parser p
failParser = Parser $ \_ -> Nothing

-- APPLY ONE OR MORE TIME THE PARSER
manyP :: Parser a -> Parser [a]
manyP p = liftM2 (:) p (many p)


charParser :: Char -> Parser Char
charParser c = Parser $ \s ->
  case s of
    (x:xs) | x == c -> Just (c, xs)
    _ -> Nothing


exprParser :: Parser Expr
exprParser = applicationParser <|> atomicParser


atomicParser :: Parser Expr
atomicParser = varParser <|> (charParser ' ' *> atomicParser) <|> parenthesisParser <|> functionParser <|> macroParser


varParser :: Parser Expr
varParser = do
  name <- manyP (predicateParser isAlpha)
  return (Variable name)


functionParser :: Parser Expr
functionParser = do
  _ <- charParser '\\'
  name <- manyP (predicateParser isAlpha)
  _ <- charParser '.'
  expr <- atomicParser
  return (Function name expr)


applicationParser :: Parser Expr
applicationParser = do
  exprs <- manyP atomicParser
  return (foldl1 Application exprs)


macroParser :: Parser Expr
macroParser = do
  _ <- charParser '$'
  name <- many (predicateParser isAlpha)
  return (Macro name)  


parenthesisParser :: Parser Expr
parenthesisParser = do
  _ <- charParser '('
  expr <- exprParser
  _ <- charParser ')'
  return expr


predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s ->
    case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing


-- TODO 4.2. parse code
parse_code :: String -> Code
parse_code inputString = 
  case parse codeParser inputString of
    Just (code, "") -> code
    _ -> error "ERROR: could not parse code!"


codeParser :: Parser Code
codeParser = assignParser <|> evalParser

assignParser :: Parser Code
assignParser = do
  name <- manyP (predicateParser isAlpha)
  white_space_before <- many (predicateParser isSpace) 
  _ <- charParser '='
  white_space_after <- many (predicateParser isSpace)
  expr <- exprParser
  return (Assign name expr)


evalParser :: Parser Code
evalParser = do
  expr <- exprParser
  return (Evaluate expr)
