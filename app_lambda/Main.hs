module Main where

import Redex (eval)
import Parser (term)
import DynSem (redex)

import Text.Parsec (parse)
import System.IO (hFlush, stdout)

main :: IO ()
main = do putStr "-> "
          hFlush stdout
          inp <- getLine
          case parse term "" inp of
            Left err -> putStrLn $ "error: " ++ show err
            Right t -> putStrLn $ show $ eval redex t
          main
