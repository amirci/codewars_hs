module SimplifyPoly where

--import Data.List.Split
import Data.List
import Data.Maybe
import Debug.Trace

import Text.Regex
import Text.Read

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)

simplify :: String -> String
simplify str = str

splitTerms = next . matchRegexAll rx 
  where
    rx = mkRegex "([+-]?[0-9]*|[a-z]+)"
    next (Just (_, match, [], _)) = [match]
    next (Just (_, match, after, _)) = match : splitTerms after
    next _ = []

int = rd <$> number
  where 
    number = many1 digit
    rd = (readMaybe :: String -> Maybe Int)


sign = toSign <$> (plus <|> minus)
  where
    plus = char '+'
    minus = char '-'
    toSign '+' = 1
    toSign '-' = -1


var :: Parser String
var = many1 lower

term = do
  sg <- option 1 sign
  qt <- option (Just 1) int
  vr <- var
  return (sg * (fromJust qt), vr)

terms = many1 term

