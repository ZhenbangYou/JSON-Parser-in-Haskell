module Main (main) where

import Lexer 
import Parser

main :: IO ()
main = do
  s <- readFile "test.json"
  let tokens = lexer s
  let ast = parser tokens
  print ast