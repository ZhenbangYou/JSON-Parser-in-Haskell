module Main (main) where

import Lexer
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  file : _ <- getArgs
  s <- readFile file
  let tokens = lexer s
  let ast = parser tokens
  print ast