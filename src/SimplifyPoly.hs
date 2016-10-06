module SimplifyPoly where

import Data.List
import Data.Maybe
import Text.Read

simplify :: String -> String
simplify = rmPlus . concatMap toTerm . solve . terms
  where
    solve = sortBy shortest . map sumTerms . groupBy sameFst . sort 
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

terms = fromMaybe [] . fmap mkTuple . parseTerm
  where
    mkTuple (rest, term) = term : terms rest

parseTerm "" = Nothing
parseTerm str = Just (rest2, (var, num))
  where
    (sg, rest) = parseSign str
    (qt, rest1) = parseNum rest
    (var, rest2) = parseVar rest1
    num = sg * qt

parseSign (a:xs)
  | a == '+' = (1, xs)
  | a == '-' = (-1, xs)
  | otherwise = (1, a:xs)

parseNum str = ((toNum digits), rest)
  where
    digits = takeWhile isDigit str
    rest = drop (length digits) str
    isDigit = flip elem ['0'..'9']
    toNum "" = 1
    toNum xs = read xs

parseVar str = (var, rest)
  where
    isChar = flip elem ['a'..'z']
    var = sort $ takeWhile isChar str
    rest = drop (length var) str

