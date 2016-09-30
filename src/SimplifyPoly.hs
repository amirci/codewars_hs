module SimplifyPoly where

import Data.List
import Data.Maybe

import Text.Read

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)

simplify :: String -> String
simplify str = rmFirstPlus $ concatMap toTerm $ solve $ mkTerms str
  where
    solve = sortBy shortest . map sumTerms . groupBy sameFst . sort 
    shortest (a, _) (b, _) = compare (length a) (length b)
    sumTerms = foldl1 (\(v, n1) (_, n2) -> (v, n1 + n2))
    sameFst (a, _) (b, _) = a == b
    mkTerms = unwrap . parse terms ""
    rmFirstPlus = dropWhile (== '+')
    unwrap (Right ts) = ts
    unwrap _ = []
    toTerm (var, n)
      | n == 1 = '+' : var
      | n >  1 = '+' : show n ++ var
      | n == 0 = ""
      | n == -1 = '-' : var
      | otherwise = show n ++ var


int = rd <$> number
  where
    number = many1 digit
    rd = (read :: String -> Int)

sign = toSign <$> (plus <|> minus)
  where
    plus = char '+'
    minus = char '-'
    toSign '+' = 1
    toSign '-' = -1

var = many1 lower

term = do
  sg <- option 1 sign
  qt <- option 1 int
  vr <- var
  return (sort vr, sg * qt)

terms = many1 term

