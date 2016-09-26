module HashTag where

import Data.List.Split
import Data.Maybe
import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag = maybeHashtag . camelize . words
  where 
    camelize = concatMap (\(x:xs) -> (toUpper x):xs)
    maybeHashtag xs
      | length xs == 0  = Nothing
      | length xs > 139 = Nothing
      | otherwise       = Just ('#':xs)
