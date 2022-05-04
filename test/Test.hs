{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Array
import Data.Ratio
import Data.Complex
import Data.List
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck
import Test.SmallCheck.Series
import Scheme
import GHC.Generics

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Scheme" [prop_parser]

data Operator = Plus
              | Sub
              | Mul
              | Div
              | Mod
              | Quot
              | Rem
              deriving (Generic)
instance Show Operator where
  show s = case s of
             Plus -> "+"
             Sub -> "-"
             Mul -> "*"
             Div -> "/"
             Mod -> "mod"
             Quot -> "quotient"
             Rem -> "remainder"
instance Monad m => Serial m Operator

data ExprExample = XString String | XList [NonNegative Integer] | XSimple (NonNegative Integer) deriving (Generic)
instance Show ExprExample where
  show s = case s of
             XString str -> "\"" ++ str ++ "\""
             XList xs -> "(" ++ unwords (map (show . getNonNegative) xs) ++ ")"
             XSimple n -> show (getNonNegative n)
instance Monad m => Serial m ExprExample

toLispVal :: ExprExample -> LispVal
toLispVal e = case e of
                XString s -> String s
                XList xs -> List [Number (getNonNegative x) | x <- xs]
                XSimple n -> Number (getNonNegative n)

prop_parser = testGroup "parser"
  [ SC.testProperty "parses a number number" $
      \x ->
        let val = getNonNegative (x :: NonNegative Integer)
        in readExpr (show val) == Number val
  , SC.testProperty "parses a complex number" $
      \(r, i) ->
        let realVal = getPositive (r :: (Positive Double))
            imgVal = getPositive (i :: (Positive Double))
        in readExpr (show realVal ++ "+" ++ show imgVal ++ "i") == Complex (realVal :+ imgVal)
  , SC.testProperty "parses a rational number" $
      \(x, y) ->
        let n = getPositive (x :: Positive Integer)
            d = getPositive (y :: Positive Integer)
        in readExpr (show n ++ "/" ++ show d) == Ratio (n % d)
  , SC.testProperty "parses a boolean" $
      \x ->
        readExpr (if (x :: Bool) then "#t" else "#f") == Bool x
  , SC.testProperty "parses a list" $
      \xs ->
        let ns = map getNonNegative (xs :: [NonNegative Integer])
        in readExpr ("(" ++ unwords (map show ns) ++ ")") == List (map Number ns)
  , SC.testProperty "parses a dotted list" $
      \xs ->
        let ns = map getNonNegative $ getNonEmpty (xs :: NonEmpty (NonNegative Integer))
            vs = foldl (\b a -> b ++ "(" ++ show a ++ " . ") "" ns ++ "nil" ++ replicate (length ns) ')'
            fld [x] = DottedList [Number x] (Atom "nil")
            fld (h:rs) = DottedList [Number h] (fld rs)
        in readExpr vs == fld ns
  , SC.testProperty "parses a quasiquoted expr" $
      \(x, a, b, c, d) ->
        let a' = getNonNegative (a :: NonNegative Integer)
            b' = getNonNegative (b :: NonNegative Integer)
            c' = getNonNegative (c :: NonNegative Integer)
            d' = getNonNegative (d :: NonNegative Integer)
            op = show (x :: Operator)
            v = "`(" ++ show a' ++ " " ++ show b' ++ " ,(" ++ op ++ " " ++ show c' ++ " " ++ show d' ++ "))"
            expected = List [Atom "quasiquote",List [Number a',Number b',List [Atom "unquote", List [Atom op,Number c',Number d']]]]
        in readExpr v == expected
  , SC.testProperty "parses a vector" $
      \xs ->
        let vs = getNonEmpty (xs :: NonEmpty ExprExample)
            s = "#(" ++ unwords (map show vs) ++ ")"
            expected = Vector  (array (0,length vs - 1) (zip [0..] (map toLispVal vs)))
        in readExpr s == expected
  , SC.testProperty "parses a quoted expr" $
      \x ->
        let v = getNonNegative (x :: NonNegative Integer)
        in readExpr ("'" ++ show v) == List [Atom "quote",Number v]
  ]

-- prop_eval = testGroup "eval"
--   [ SC.testProperty "evaluates binary operator" $
--       \op a b ->
--         let biop =
--   ]
