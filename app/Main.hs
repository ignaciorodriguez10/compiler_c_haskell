module Main where

import Tokens

main = do
  content <- readFile "app/example"
  let tokens = scanTokens content
  mapM_ print tokens