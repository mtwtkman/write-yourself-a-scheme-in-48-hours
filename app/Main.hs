module Main where

import Scheme
import System.Environment
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  let evaluated = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaluated
