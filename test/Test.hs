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
properties = testGroup "Scheme" [ prop_parser
                                , prop_eval
                                ]

data BinaryOperator = Plus
              | Sub
              | Mul
              | Div
              | Mod
              | Quot
              | Rem
              deriving (Generic)
instance Show BinaryOperator where
  show s = case s of
             Plus -> "+"
             Sub -> "-"
             Mul -> "*"
             Div -> "/"
             Mod -> "mod"
             Quot -> "quotient"
             Rem -> "remainder"
instance Monad m => Serial m BinaryOperator

toHaskellBinaryOperator :: BinaryOperator -> (Integer -> Integer -> Integer)
toHaskellBinaryOperator op = case op of
                               Plus -> (+)
                               Sub -> (-)
                               Mul -> (*)
                               Div -> div
                               Mod -> mod
                               Quot -> quot
                               Rem -> rem


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
        in readExpr (show val) == Right (Number val)
  , SC.testProperty "parses a complex number" $
      \(r, i) ->
        let realVal = getPositive (r :: (Positive Double))
            imgVal = getPositive (i :: (Positive Double))
        in readExpr (show realVal ++ "+" ++ show imgVal ++ "i") == Right (Complex (realVal :+ imgVal))
  , SC.testProperty "parses a rational number" $
      \(x, y) ->
        let n = getPositive (x :: Positive Integer)
            d = getPositive (y :: Positive Integer)
        in readExpr (show n ++ "/" ++ show d) == Right (Ratio (n % d))
  , SC.testProperty "parses a boolean" $
      \x ->
        readExpr (if (x :: Bool) then "#t" else "#f") == Right (Bool x)
  , SC.testProperty "parses a list" $
      \xs ->
        let ns = map getNonNegative (xs :: [NonNegative Integer])
        in readExpr ("(" ++ unwords (map show ns) ++ ")") == Right (List (map Number ns))
  , SC.testProperty "parses a dotted list" $
      \xs ->
        let ns = map getNonNegative $ getNonEmpty (xs :: NonEmpty (NonNegative Integer))
            vs = foldl (\b a -> b ++ "(" ++ show a ++ " . ") "" ns ++ "nil" ++ replicate (length ns) ')'
            fld [x] = DottedList [Number x] (Atom "nil")
            fld (h:rs) = DottedList [Number h] (fld rs)
        in readExpr vs == Right (fld ns)
  , SC.testProperty "parses a quasiquoted expr" $
      \(x, a, b, c, d) ->
        let a' = getNonNegative (a :: NonNegative Integer)
            b' = getNonNegative (b :: NonNegative Integer)
            c' = getNonNegative (c :: NonNegative Integer)
            d' = getNonNegative (d :: NonNegative Integer)
            op = show (x :: BinaryOperator)
            v = "`(" ++ show a' ++ " " ++ show b' ++ " ,(" ++ op ++ " " ++ show c' ++ " " ++ show d' ++ "))"
            expected = List [Atom "quasiquote",List [Number a',Number b',List [Atom "unquote", List [Atom op,Number c',Number d']]]]
        in readExpr v == Right (expected)
  , SC.testProperty "parses a vector" $
      \xs ->
        let vs = getNonEmpty (xs :: NonEmpty ExprExample)
            s = "#(" ++ unwords (map show vs) ++ ")"
            expected = Vector  (array (0,length vs - 1) (zip [0..] (map toLispVal vs)))
        in readExpr s == Right (expected)
  , SC.testProperty "parses a quoted expr" $
      \x ->
        let v = getNonNegative (x :: NonNegative Integer)
        in readExpr ("'" ++ show v) == Right (List [Atom "quote",Number v])
  ]

prop_eval = testGroup "eval"
  [ SC.testProperty "evaluates binary operator" $
      \op a b ->
        let op' = Atom $ show (op :: BinaryOperator)
            a' = getPositive (a :: Positive Integer)
            b' = getPositive (b :: Positive Integer)
         in eval (List [op',Number a',Number b']) == Right (Number (toHaskellBinaryOperator op a' b'))
  , SC.testProperty "evaluates string" $
      \x ->
        eval (String (x :: String)) == Right (String x)
  , SC.testProperty "evaluates number" $
      \x ->
        let v = getNonNegative (x :: NonNegative Integer)
        in eval (Number v) == Right (Number v)
  , SC.testProperty "evaluates bool" $
      \x ->
        eval (Bool (x :: Bool)) == Right (Bool x)
  , SC.testProperty "evaluates quote" $
      \x ->
        eval (List [Atom "quote",String (x :: String)]) == Right (String x)
  , SC.testProperty "evaluates if expression" $
      \x ->
        let truthy = String "true"
            falsy = String "false"
            expected = if (x :: Bool) then truthy  else falsy
         in eval (List [Atom "if",Bool x,truthy,falsy]) == Right expected
  ]
