module Main where

import System.IO
import Tokens
import Grammar
import Lib

main :: IO ()
main = do
    content <- readFile "app/example"
    let tokens = scanTokens content
    putStrLn "Tokens:"
    mapM_ (putStrLn . show) tokens
    putStrLn "--------------------------------------------------------------------------------------------"

    _ <- getLine -- Esperar a que el usuario introduzca cualquier letra y pulse Enter
    putStrLn "\nARBOL SINTACTICO:"
    let ast = parse tokens
    print ast
    putStrLn "--------------------------------------------------------------------------------------------"
    _ <- getLine -- Esperar a que el usuario introduzca cualquier letra y pulse Enter

    putStrLn "INTERPRETACIÓN:"
    interpretar ast
