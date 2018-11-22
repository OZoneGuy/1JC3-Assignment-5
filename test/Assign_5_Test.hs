{- Assignment 5 Tests
 - Name: TODO add full name
 - Date: TODO add of completion
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1(definiteIntegral -> Odd Symmetry): "
          quickCheck definiteIntegralPropOdd
          print "Performing Test 2(definiteIntegral -> Even Symmetry):"
          quickCheck definiteIntegralPropEven
          print "Performing Test 3(definiteIntegral):"
          quickCheck definiteIntegralProp3
          print "Performing Test 4 (FunH):"
          quickCheck funHProp1
          print "Performing Test 5 (FunH):"
          quickCheck funHProp2
          print "Performing Test 6 (funK):"
          quickCheck funKProp1

definiteIntegralPropOdd :: Double -> Integer -> Bool
definiteIntegralPropOdd a n = n<=0 || (abs(definiteIntegral (-a) a id n) < 10E-5)
definiteIntegralPropEven :: Double -> Integer -> Bool
definiteIntegralPropEven a n = (n<=0) ||(n `mod`2 ==1 || abs (definiteIntegral (-a) a abs n - (2*definiteIntegral 0 a abs n)) < 10E-1)
definiteIntegralProp3 :: Double -> Double -> Double -> Integer -> Bool
definiteIntegralProp3 a b c n = n<=0 || abs((definiteIntegral a c id n + definiteIntegral c b id n) - definiteIntegral a b id n) < 10E-5

funHProp1 :: Integer -> Bool
funHProp1 n = (n<=1) || funH n > 0
funHProp2 :: Integer -> Bool
funHProp2 n = n<=0 || (n>=1 || funH n < 0)

funKProp1 :: Double -> Bool
funKProp1 n = n <=0 || abs(funK (1/n) - funK n) < 10E-5
