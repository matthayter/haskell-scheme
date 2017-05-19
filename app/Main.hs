module Main where

import Lib
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> case val of
        String s -> "Found string with length " ++ show (length s) ++ ": " ++ s
        _ -> "Found: " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (literalQuote <|> (noneOf "\""))
    char '"'
    return $ String x

literalQuote :: Parser Char
literalQuote = string "\\\"" >> return '"'

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

