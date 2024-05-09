module Main where

import Tokens
import Grammar
import Lib


main :: IO ()
main = do
    content <- readFile "app/example"
    let tokens = scanTokens content
    let ast = parse tokens
    interpretar ast