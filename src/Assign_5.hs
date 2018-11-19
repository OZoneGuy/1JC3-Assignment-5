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
definiteIntegral a b g n | n >= 0  = let
                          dX = (b-a)/fromIntegral n
                          xi1 = a+dX*fromIntegral(n-1)
                          xi2 = a+dX*fromIntegral n
                          h = (g xi1 + g xi2)/2
                          in if n == 1
                            then h*dX
                            else definiteIntegral a b g (n-1) + h*dX

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: Estimates the area between x^(1/n) and x^n using the definiteIntegral function from 0 to 1
 -}
funH :: Integer -> Double
funH n | n > 0 = let
  f x = x**(1/fromIntegral n) - x**fromIntegral n
  in abs(definiteIntegral 0 1 f n)

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: Estimate the are between n^x and the x-axis using the definiteIntegral function from -1 to 1
 -}
funK :: Double -> Double
funK n | n > 0 = abs(definiteIntegral (-1) 1 (\x->n**x) 5)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function:
 - - Test Case Number:
 - - Input:
 - - Expected Output:
 - - Acutal Output:
 - -----------------------------------------------------------------
 - TODO: add test cases
 -}
