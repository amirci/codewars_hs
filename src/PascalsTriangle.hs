module PascalsTriangle where

pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat $ take n $ iterate pascal [1]
  where
    pascal prev = (:) 1 $ newRow prev ++ [1]
    newRow old = zipWith (+) old $ drop 1 old
