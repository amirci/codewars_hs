module SimplifyPoly where

import Data.Char (isAsciiLower, isDigit)
import Data.Function (on)
import Data.List
import Data.Maybe
import Text.Read

simplify :: String -> String
simplify = rmPlus . concatMap showTerm . solve . unfoldr parseTerm
  where
    solve = sortBy shortest . map sumTerms . groupBy sameFst . sort
    shortest = compare `on` (length . fst)
    sumTerms = foldl1 (\(v, n1) (_, n2) -> (v, n1 + n2))
    sameFst = (==) `on` fst
    showTerm (var, n)
      | n == 1 = '+' : var
      | n >  1 = '+' : show n ++ var
      | n == 0 = ""
      | n == -1 = '-' : var
      | otherwise = show n ++ var

rmPlus = dropWhile ((==) '+')

parseTerm "" = Nothing
parseTerm str = Just ((var, sg * qt), rest2)
  where
    (sg, rest)   = mapFst toSign $ splitWhile isSign str
    (qt, rest1)  = mapFst toNum  $ splitWhile isDigit rest
    (var, rest2) = mapFst sort   $ splitWhile isAsciiLower rest1
    isSign = flip elem ['+', '-']
    toNum = fromMaybe 1 . readMaybe
    toSign "-" = -1
    toSign xs  = 1

mapFst fn (a, b) = ((fn a), b)

splitWhile pred str = (consumed, rest)
  where
    consumed = takeWhile pred str
    rest = drop (length consumed) str

