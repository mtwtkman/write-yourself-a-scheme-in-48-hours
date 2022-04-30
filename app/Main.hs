module Main where

import Scheme
import System.Environment

main :: IO ()
main = do
  (expr: _) <- getArgs
  print (readExpr expr)
