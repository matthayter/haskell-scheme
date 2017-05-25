module Parse where

import Types
import Text.ParserCombinators.Parsec(Parser, skipMany1, many, many1, oneOf, noneOf, (<|>))
import qualified Text.ParserCombinators.Parsec as PS
import Control.Monad(liftM)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case PS.parse parseExpr "lisp" input of
    Left err -> "No Match: " ++ show err
    Right val -> case val of
        String s -> "Found string with length " ++ show (length s) ++ ": " ++ s
        _ -> "Found: " ++ show val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseList

parseString :: Parser LispVal
parseString = do
    PS.char '"'
    x <- many (escapeSequence <|> (noneOf "\""))
    PS.char '"'
    return $ String x

escapeSequence :: Parser Char
escapeSequence = do
    PS.char '\\'
    c <- oneOf "tnr\\\""
    case c of
        't' -> return '\t'
        'n' -> return '\n'
        '\\' -> return '\\'
        'r' -> return '\r'
        '"' -> return '"'

parseAtom :: Parser LispVal
parseAtom = do
    first <- PS.letter <|> symbol
    rest <- many (PS.letter <|> PS.digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    wholeNumber <- many1 PS.digit
    let floatTail = do {
        PS.char '.';
        d <- many1 PS.digit;
        return $ wholeNumber ++ "." ++ d;
    }
    let parseFloat = liftM (Float . read) floatTail
    let parseInt = return (Number $ read wholeNumber)
    parseFloat <|> parseInt

parseList :: Parser LispVal
parseList = do
    PS.char '('
    parseList' []

parseList' :: [LispVal] -> Parser LispVal
parseList' rHead = do
    next <- parseExpr
    let head = reverse $ next : rHead
    (PS.char ')' >> (return $ List head)) <|> do
        spaces
        (PS.char '.' >> spaces >> DottedList head <$> parseExpr) <|> parseList' (next : rHead)

parseQuoted :: Parser LispVal
parseQuoted = do
    PS.char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

joinParse :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
joinParse f a b = do
    l <- a
    r <- b
    return $ f l r


spaces :: Parser ()
spaces = skipMany1 PS.space