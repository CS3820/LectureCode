{- HLINT ignore "Use camelCase" -}
{- HLINT ignore "Alternative law, left identity" -}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes     #-}
-- {-# OPTIONS_GHC -Wall #-}
module Lecture4Sheep where

import           Data.Function
import           Data.List     ((\\))
import           Prelude       hiding (words, (<>), (<|>))

alphabet :: [Char]
alphabet = ['A'..'Z'] ++ ['a'..'z']

parens :: String -> String
parens s = "(" ++ s ++ ")"

splits :: String -> [(String, String)]
splits s = map split [0..length s] where
  split i = splitAt i s

data VD = CharsV [Char] | OrV Regex_s Regex_s | EmptyV | SeqV Regex_s Regex_s

data Regex_s = Mk { format_s    :: String
                  , enumerate_s :: [String]
                  , recognize_s :: String -> Bool
                  , eat_s       :: String -> [String]
                  , visit_s     :: forall a. (VD -> a) -> a
                  }

chars_s :: [Char] -> Regex_s
chars_s cs = Mk { format_s    = "[" ++ cs ++ "]"
                , enumerate_s = enumerate
                , recognize_s = recognize
                , eat_s       = eat
                , visit_s     = visit                
                }
  where enumerate = map singleton cs where
          singleton c = [c]

        recognize [c] = c `elem` cs
        recognize _   = False

        eat (d : ds)
          | d `elem` cs = [ds]
          | otherwise   = []
        eat []          = []
        visit f         = f (Chars cs)

-- >>> (chars_s ['a', 'b', 'c'] & recognize_s) "f"
-- False

(<|>) :: Regex_s -> Regex_s -> Regex_s
p <|> q = Mk { format_s    = parens (format_s p ++ "|" ++ format_s q)
             , enumerate_s = enumerate_s p ++ enumerate_s q
             , recognize_s = recognize
             , eat_s       = eat
             }

  where recognize s = recognize_s p s || recognize_s q s
        -- eat s = eat_s p s ++ eat_s q s

-- >>> format_s ((chars_s ['a'] <|> chars_s ['b']) <|> chars_s ['c','d'])
-- "(([a]|[b])|[cd])"

empty_s :: Regex_s
empty_s = Mk { format_s    = "e"
             , enumerate_s = [""]
             , recognize_s = recognize
             , eat_s       = eat
             }
  where recognize = null
        eat s     = [s]

-- >>> format_s empty_s
-- "e"

(<>) :: Regex_s -> Regex_s -> Regex_s
p <> r = Mk { format_s = format_s p ++ format_s r
            , enumerate_s = [v ++ w | v <- enumerate_s p
                                    , w <- enumerate_s r]
            , recognize_s = recognize
            -- , eat_s       = eat
            }
  where recognize s = or [ recognize_s p front && recognize_s r back
                         | (front, back) <- splits s]
        -- eat s       = concatMap (eat_s r) (eat_s p s)

star :: Regex_s -> Regex_s
star r = Mk { format_s = format_s r ++ "*"
            , enumerate_s = "" : enumerate_s (r <> star r)
            , recognize_s = recognize
            -- , eat_s       = eat_s (empty_s <|> (r <> star r))
            }
  where recognize s = null s || recognize_s (r <> star r) s

eat :: Regex_s -> String -> [String]
eat r = visit_s r visitor where
  visitor :: VD -> String -> [String]
  visitor (CharsV cs) (d : ds) 
    | d `elem` cs = [ds]
    | otherwise   = []
  visitor (CharsV cs) [] = []



r1_s :: Regex_s
r1_s = chars_s "g" <> chars_s "a" <> ((chars_s "r" <> chars_s "r") <|> chars_s "r")

r2_s = chars_s "g" <> chars_s "a" <> star (chars_s "r")


-- >>> format_s r2_s
-- "[g][a][r]*"

-- >>> recognize_s r2_s "gart"
-- False

-- >>> eat_s r2_s "garrrr"
-- ["rrrr","rrr","rr","r",""]


-----

data Regex_d = Chars [Char] | Regex_d :|: Regex_d | Empty | Regex_d :<>: Regex_d | Star_d Regex_d

r1_d = Chars "g" :<>: Chars "a" :<>: ((Chars "r" :<>: Chars "r") :|: Chars "r")
r2_d = Chars "g" :<>: Chars "a" :<>: (Star_d (Chars "r"))


format_d :: Regex_d -> String
format_d (Chars cs) = "[" ++ cs ++ "]"
format_d (p :|: r)  = parens (format_d p ++ "|" ++ format_d r)
format_d (p :<>: r) = format_d p ++ format_d r
format_d Empty      = "e"
format_d (Star_d r) = format_d r ++ "*"

-- >>> format_d r2_d
-- "[g][a][r]*"

enumerate_d :: Regex_d -> [String]
enumerate_d (Chars cs) = map singleton cs where
  singleton c = [c]
enumerate_d (p :|: r) = enumerate_d p ++ enumerate_d r
enumerate_d (p :<>: r) = [v ++ w | v <- enumerate_d p, w <- enumerate_d r]
enumerate_d Empty = [""]
enumerate_d (Star_d r) = "" : enumerate_d (r :<>: Star_d r)

eat_d :: Regex_d -> String -> [String]
eat_d (Chars ds) (c : cs)
  | c `elem` ds = [cs]
  | otherwise   = []
eat_d (Chars ds) [] = []
eat_d (p :|: r) s = eat_d p s ++ eat_d r s
eat_d (p :<>: r) s = concatMap (eat_d r) (eat_d p s)
eat_d Empty s = [s]
eat_d (Star_d r) s = s : eat_d (r :<>: Star_d r) s


-- >>> take 20 (enumerate_d r2_d)
-- ["ga","gar","garr","garrr","garrrr","garrrrr","garrrrrr","garrrrrrr","garrrrrrrr","garrrrrrrrr","garrrrrrrrrr","garrrrrrrrrrr","garrrrrrrrrrrr","garrrrrrrrrrrrr","garrrrrrrrrrrrrr","garrrrrrrrrrrrrrr","garrrrrrrrrrrrrrrr","garrrrrrrrrrrrrrrrr","garrrrrrrrrrrrrrrrrr","garrrrrrrrrrrrrrrrrrr"]

