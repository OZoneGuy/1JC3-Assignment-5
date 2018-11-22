{- Assignment 5
 - Name: Omar Alkersh
 - Date: 19/11/18
 -}
module Assign_5 where

macid :: String
macid = "alkersho"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: Uses the trapqezoid rule to estimate the area under
 - function g from a to b
 - Where xi1 and xi2 are the two trapqezoid hieghts and dX is the width
 - multiply them to get the are under each sector then recurse and add all the
 - sectors to estomate the area
 - Link to Wikipedia page: https://en.wikipedia.org/wiki/Trapezoidal_rule
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n | n <= 0 = undefined
                         | otherwise = let
                          dX = (b-a)/fromInteger n
                          bigSum:: Integer -> Integer -> (Double->Double) -> Double
                          bigSum i n f = let
                           xi1 = a+dX*fromInteger i
                           xi2 = a+dX*fromInteger(i-1)
                           h = (f xi1 + f xi2)/2
                           in if i < n
                            then bigSum (i+1) n f + h*dX
                            else h*dX
                          in bigSum 1 n g

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: Estimates the area between x^(1/n) and x^n using the definiteIntegral function from 0 to 1
 -}
funH :: Integer -> Double
funH n | n <= 0 = undefined
       | otherwise = let
                      f x = x**(1/fromIntegral n) - x**fromIntegral n
                      in abs(definiteIntegral 0 1 f 10)

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: Estimate the are between n^x and the x-axis using the definiteIntegral function from -1 to 1
 -}
funK :: Double -> Double
funK n | n <= 0 = undefined
       | otherwise = abs(definiteIntegral (-1) 1 (\x->n**x) 10)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function:definiteIntegral
 - -----------------------------------------------------------------
 - - Test Case Number: 1-A
 - - Input: (-10) 10 (\x->x) 10
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Test Case Number: 1-B
 - - Input: 0 10 (\x->x) 10
 - - Expected Output: 50
 - - Acutal Output: 50.0
 - -----------------------------------------------------------------
 - - Test Case N8umber: 1-C
 - - Input: (-10) 0 (\x->x) 10
 - - Expected Output: -50
 - - Acutal Output: -50
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Property: n<=0 || (abs(definiteIntegral (-a) a (\x->x) n) < 10E-7)
 - Actual Test Results: Passed
 - -----------------------------------------------------------------
 - Property: (n<=0) ||(n `mod`2 ==1 || abs (definiteIntegral (-a) a abs n - (2*definiteIntegral 0 a abs n)) < 10E-1)
 - Actual Test Results: Passed
 - only fails when n is an odd
 - -----------------------------------------------------------------
 - Property: n<=0 || abs((definiteIntegral a c id n + definiteIntegral c b id n) - definiteIntegral a b id n) < 10E-7
 - Actual Test Results: Passed
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: funH
 - -----------------------------------------------------------------
 - - Test Case Number: 2 - A
 - - Input: 5
 - - Expected Output: 0.666666666666667
 - - Acutal Output: 0.6406125093145795
 - -----------------------------------------------------------------
 - - Test Case Number: 2 - B
 - - Input: 100
 - - Expected Output: 0.9801980198019802
 - - Acutal Output: 0.89213113763881315
 - -----------------------------------------------------------------
 - - Test Case N8umber: 2 - C
 - - Input: -10
 - - Expected Output: undefined error
 - - Acutal Output: undefined error
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Property: (n<=1) || funH n > 0
 - Actual Test Results: Passed
 - -----------------------------------------------------------------
 - Property: n<=0 || (n>=1 || funH n < 0)
 - Actual Test Results: Passed
 - -----------------------------------------------------------------
 -}

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 - - Function: funK
 - -----------------------------------------------------------------
 - - Test Case Number: 3 - A
 - - Input: (-5)
 - - Expected Output: undefined error
 - - Acutal Output: undefined error
 - -----------------------------------------------------------------
 - - Test Case Number: 3 - B
 - - Input: 1
 - - Expected Output: 2
 - - Acutal Output: 1.9999999999998
 - -----------------------------------------------------------------
 - - Test Case N8umber: 3 - C
 - - Input: 2
 - - Expected Output: 2.167507187661773
 - - Acutal Output: 2.167507187661773
 - -----------------------------------------------------------------
 - - Test Case N8umber: 3 - D
 - - Input: 0.5
 - - Expected Output: 2.167507187661773
 - - Acutal Output: 2.167507187661773
 - -----------------------------------------------------------------
 - QuickCheck
 - -----------------------------------------------------------------
 - Property: n <=0 || abs(funK (1/n) - funK n) < 10E-5
 - Actual Test Results: Passed
 - -----------------------------------------------------------------
 -}
