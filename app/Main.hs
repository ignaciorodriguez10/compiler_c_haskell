module Main where

import Tokens
import Grammar
import Lib


main :: IO ()
main = do
    content <- readFile "app/example"
    let tokens = scanTokens content
    mapM_ print tokens
    let ast = parse tokens
    print ast 
    mapM_ print " "
    mapM_ print " "
    mapM_ print " "
    mapM_ print " "

    interpretar ast