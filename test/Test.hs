{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

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
properties = testGroup "Props" [prop]

data Operator = Plus | Sub | Mul | Div deriving (Generic)
instance Show Operator where
  show s = case s of
             Plus -> "+"
             Sub -> "-"
             Mul -> "*"
             Div -> "%"
instance Monad m => Serial m Operator where

prop = testGroup "MyScheme works fine"
  [ SC.testProperty "parsing a number number" $
      \x ->
        let val = getNonNegative (x :: NonNegative Integer)
        in readExpr (show val) == Number val
  , SC.testProperty "parsing a complex number" $
      \(r, i) ->
        let realVal = getPositive (r :: (Positive Double))
            imgVal = getPositive (i :: (Positive Double))
        in readExpr (show realVal ++ "+" ++ show imgVal ++ "i") == Complex (realVal :+ imgVal)
  , SC.testProperty "parsing a rational number" $
      \(x, y) ->
        let n = getPositive (x :: Positive Integer)
            d = getPositive (y :: Positive Integer)
        in readExpr (show n ++ "/" ++ show d) == Ratio (n % d)
  , SC.testProperty "parsing a boolean" $
      \x ->
        readExpr (if (x :: Bool) then "#t" else "#f") == Bool x
  , SC.testProperty "parsing a list" $
      \xs ->
        let ns = map getNonNegative (xs :: [NonNegative Integer])
        in readExpr ("(" ++ unwords (map show ns) ++ ")") == List (map Number ns)
  , SC.testProperty "parsing a dotted list" $
      \xs ->
        let ns = map getNonNegative $ getNonEmpty (xs :: NonEmpty (NonNegative Integer))
            vs = foldl (\b a -> b ++ "(" ++ show a ++ " . ") "" ns ++ "nil" ++ replicate (length ns) ')'
            fld [x] = DottedList [Number x] (Atom "nil")
            fld (h:rs) = DottedList [Number h] (fld rs)
        in readExpr vs == fld ns
  , SC.testProperty "parsing quasiquoted expr" $
      \(x, a, b, c, d) ->
        let a' = getNonNegative (a :: NonNegative Integer)
            b' = getNonNegative (b :: NonNegative Integer)
            c' = getNonNegative (c :: NonNegative Integer)
            d' = getNonNegative (d :: NonNegative Integer)
            op = show (x :: Operator)
            v = "`(" ++ show a' ++ " " ++ show b' ++ " ,(" ++ op ++ " " ++ show c' ++ " " ++ show d' ++ "))"
            expected = List [Atom "quasiquote",List [Number a',Number b',List [Atom "unquote", List [Atom op,Number c',Number d']]]]
        in readExpr v == expected
  ]
