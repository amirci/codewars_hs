module SimplifyPoly where

import Data.List
import Data.Maybe
import Text.Regex
import Debug.Trace
import Text.Read

simplify :: String -> String
simplify str = rmPlus $ concatMap toTerm $ solve $ mkTerms str
  where
    solve = sortBy shortest . map sumTerms . groupBy sameFst . sort 
    mkTerms = map parse . terms
    shortest (a, _) (b, _) = compare (length a) (length b)
    sumTerms = foldl1 (\(v, n1) (_, n2) -> (v, n1 + n2))
    sameFst (a, _) (b, _) = a == b
    toTerm (var, n)
      | n == 1 = '+' : var
      | n >  1 = '+' : show n ++ var
      | n == 0 = ""
      | n == -1 = '-' : var
      | otherwise = show n ++ var

rmPlus = dropWhile ((==) '+')

terms "" = []
terms str = fromJust $ fmap mkTuple $ matchRegexAll re str
  where
    re = mkRegex "([\\+-]?[0-9]*)([a-z]+)"
    mkTuple (_, _, rest, [num, var]) = (var, num) : terms rest

parse (var, num) = ((sort var), (parse' num))
  where
    parse' ""  = 1
    parse' "+" = 1
    parse' "-" = -1
    parse' num = rd $ rmPlus num
    rd = read :: String -> Int
