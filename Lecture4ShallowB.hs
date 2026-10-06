{- HLINT ignore -}
{-# LANGUAGE FlexibleInstances #-}
module Lecture4ShallowFull where

import           Data.List ((\\))
import           Prelude   hiding (words, (<>), (<|>))



alphabet :: [Char]
alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ [' ', '\t', '\r', '\n']

class Regex r where
  literal :: Char -> r
  (<|>)   :: r -> r -> r
  none    :: r
  (<>)    :: r -> r -> r
  empty   :: r

instance Regex [String] where
  literal c = [[c]]
  p <|> r = p ++ r
  none = []
  ps <> qs = -- [p ++ q | p <- ps, q <- qs]
    concatMap (one ps) qs where
    one [] _       = []
    one (p : ps) q = (p ++ q) : one ps q
  empty = [""]

instance Regex (String -> Bool) where
  literal c s = s == [c]
  (p <|> r) s = p s || r s
  none s = False
  (p <> q) s = or [p begin && q end | (begin, end) <- splits s]
    where
    splits :: String -> [(String, String)]
    splits [] = [([], [])]
    splits (c : cs) =
      ([], c : cs) :
      [(c : begin, end) | (begin, end) <- splits cs]
  empty s = null s

instance Regex (String -> [String]) where
  literal c (d : ds) | c == d = [ds]
  literal _ _                 = []

  (p <|> r) s = p s ++ r s
  none s = []

  (p <> q) s = (concatMap q . p) s

  empty s = [s]

-- >>> (upper <> star lower :: String -> [String]) "DAb"
-- ["Ab"]



{-
Questions we could ask:

  1a. if x `elem` (r :: [String]) == True, then is (r :: String -> Bool) x == True?
  1b. if x `elem` (r :: [String]) == False, then is (r :: String -> Bool) x == False?
  2a. if (r :: String -> Bool) x == True, then does x `elem` (r :: [String]) == True?
  2b. if (r :: String -> Bool) x == False, then does x `elem` (r :: [String]) == False?

-}

chars :: Regex r => [Char] -> r
chars cs = union (map literal cs)

notChars :: Regex r => [Char] -> r
notChars cs = chars (alphabet \\ cs)

anyChars :: Regex r => r
anyChars = chars alphabet

union :: Regex r =>  [r] -> r
union []       = none
union (r : rs) = r <|> union rs

concatenate :: Regex r => [r] -> r
concatenate []       = empty
concatenate (r : rs) = r <> concatenate rs

question, star, plus :: Regex r => r -> r
question r = empty <|> r
plus r = r <> star r
star r = question (plus r)

-----------------------------------------

upper, lower, space :: Regex r => r -- String -> Bool
upper = chars ['A'..'Z']
lower = chars ['a'..'z']
space = chars [' ', '\t', '\r', '\n']

-- >>> (upper <> star lower :: String -> Bool) ("A" ++ replicate 10000 'b')
-- True

-- >>> take 50 (upper <> star lower :: [String])
-- ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","Aa","Ba","Ca","Da","Ea","Fa","Ga","Ha","Ia","Ja","Ka","La","Ma","Na","Oa","Pa","Qa","Ra","Sa","Ta","Ua","Va","Wa","Xa"]
