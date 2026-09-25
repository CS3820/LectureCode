{- HLINT ignore -}
{-# LANGUAGE FlexibleInstances #-}
module Lecture4ShallowFull where

import           Data.List ((\\))
import           Prelude   hiding (words, (<>), (<|>))

-- Strategy #2: regular expressions are functions from String to Bool

-- type Regex = String -> Bool

alphabet :: [Char]
alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ [' ', '\t', '\r', '\n']

-- >>> length alphabet
-- 66

class Regex r where
  literal :: Char -> r
  (<|>)   :: r -> r -> r
  none    :: r
  (<>)    :: r -> r -> r
  empty   :: r

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

-- literal :: Char -> Regex   -- == Char -> String -> Bool
-- literal c s = s == [c]

-- chars ['a', 'b', 'c'] = ["a", "b", "c"]
chars :: Regex r => [Char] -> r
chars cs = union (map literal cs)

-- chars []       = none
-- chars (c : cs) = literal c <|> chars cs

-- chars = concatMap literal

-- chars []       = []
-- chars (c : cs) = [c] : chars cs

-- >>> chars ['a', 'e', 'd']
-- ["a","e","d"]

notChars :: Regex r => [Char] -> r
notChars cs = chars (alphabet \\ cs)

-- >>> notChars ['a', 'b', 'c'] "c"
-- False

anyChars :: Regex r => r
anyChars = chars alphabet

--------------------------------------------------
-- Alternation

-- (<|>) :: Regex -> Regex -> Regex
-- --    == (String -> Bool) -> (String -> Bool) -> String -> Bool
-- (p <|> r) s = p s || r s

-- >>> (literal 'a' <|> literal 'b') "aa"
-- False


-- Want a regular expression `R` such that for any other regex p, R <|> p == p

-- none :: Regex
-- none s = False

{-

  p <|> none = p

by funext:

  (p <|> none) s
= p s || none s
= p s || False
= p s

-}

union :: Regex r =>  [r] -> r
union []       = none
union (r : rs) = r <|> union rs


-------------------------------------------------
-- Sequencing

-- (<>) :: Regex -> Regex -> Regex
-- --   == (String -> Bool) -> (String -> Bool) -> String -> Bool
-- (p <> q) s = or [p begin && q end | (begin, end) <- splits s]
--   where
--   splits :: String -> [(String, String)]
--   splits [] = [([], [])]
--   splits (c : cs) =
--     ([], c : cs) :
--     [(c : begin, end) | (begin, end) <- splits cs]

    {-
      "abcd"
      'a' : "bcd"
         "", "abcd"  <-- new
         "a", "bcd"  <-- 'a' + some existing split
         "ab", "cd"
         "abc", "d"
         "abcd", ""
    -}


-- >>> (literal 'a' <> literal 'b') "abb"
-- False

-- >>> (chars ['D', 'E'] <> chars ['A', 'B', 'C']) "AD"
-- False

-- empty :: Regex
-- empty s = null s

concatenate :: Regex r => [r] -> r
concatenate []       = empty
concatenate (r : rs) = r <> concatenate rs

-- >>> concatenate [chars ['a', 'b'], chars ['c', 'd'], chars ['e', 'f']]
-- ["ace","acf","ade","adf","bce","bcf","bde","bdf"]

{-

How are <|> and <> related?
===========================

(literal 'a' <|> literal 'b') <> literal 'c'
==
(literal 'a' <> literal 'c') <|> (literal 'b' <> literal 'c')

(literal 'a' <> literal 'b') <|> (literal 'c' <> literal 'd')
=/=
(literal 'a' <|> literal 'c') <> (literal 'b' <|> literal 'd')

-}

-------------------------------------------
-- Repetition (and option)

question, star, plus :: Regex r => r -> r

question r = empty <|> r

plus r = r <> star r

star r = question (plus r)

-----------------------------------------

upper, lower, space :: String -> Bool
upper = chars ['A'..'Z']
lower = chars ['a'..'z']
space = chars [' ', '\t', '\r', '\n']

-- >>> (upper <> star lower) ("A" ++ replicate 10000 'b')
-- True

