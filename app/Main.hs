module Main(main) where

import Lib
import Parse
import Types
import System.Environment
import Control.Monad

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr

-- Assistance for running in ghci.
p x = putStr $ x ++ "\n"

runOn = do
    s <- getLine
    p $ readExpr s