module SumByFactors where

import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace

sumByFac :: [Integer] -> [[Integer]]
sumByFac numbers = sort $ map sumFac factors
  where 
    factors = nub $ concatMap primeFactors numbers
    sumFac f = [f, sum $ filter (isFactor f) numbers]

isFactor :: Integer -> Integer -> Bool
isFactor x n = (n `mod` x) == 0

primeFactors :: Integer -> [Integer]
primeFactors n =
  case factors of
    []     -> [n']
    (a:xs) -> factors ++ primeFactors (n' `div` a)
  where 
    factors = take 1 $ filter (flip isFactor n') [truncate i | i <- [2 .. limit]]
    n' = abs n
    limit = sqrt $ fromIntegral n' :: Double

