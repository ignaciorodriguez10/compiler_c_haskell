module Main where

import Tokens
import Grammar

main = do
  content <- readFile "app/example"
  let tokens = scanTokens content
  mapM_ print tokens
  let ast = parse tokens
  print ast
