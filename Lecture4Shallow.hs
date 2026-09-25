{- HLINT ignore -}
module Lecture4ShallowFull where

import           Data.List ((\\))
import           Prelude   hiding (words, (<>), (<|>))

-- Strategy #1: regular expressions are sets of strings

type Regex = [String]

alphabet :: [Char]
alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ [' ', '\t', '\r', '\n']

-- >>> length alphabet
-- 66

literal :: Char -> Regex
literal c = [[c]]

-- chars ['a', 'b', 'c'] = ["a", "b", "c"]
chars :: [Char] -> Regex
chars cs = union (map literal cs)

-- chars []       = none
-- chars (c : cs) = literal c <|> chars cs

-- chars = concatMap literal

-- chars []       = []
-- chars (c : cs) = [c] : chars cs

-- >>> chars ['a', 'e', 'd']
-- ["a","e","d"]

notChars :: [Char] -> Regex
notChars cs = chars (alphabet \\ cs)

-- >>> notChars ['a', 'b', 'c']
-- ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","0","1","2","3","4","5","6","7","8","9"," ","\t","\r","\n"]

anyChars :: Regex
anyChars = chars alphabet

--------------------------------------------------
-- Alternation

(<|>) :: Regex -> Regex -> Regex
p <|> r = p ++ r

-- >>> chars ['a'] <|> chars ['b']
-- ["a","b"]

-- Want a regular expression `R` such that for any other regex p, R <|> p == p

none :: Regex
none = []

union :: [Regex] -> Regex
union []       = none
union (r : rs) = r <|> union rs

-------------------------------------------------
-- Sequencing

(<>) :: Regex -> Regex -> Regex

-- []       <> qs = []
-- (p : ps) <> qs = one p qs ++ ps <> qs where

ps <> qs = concatMap (one ps) qs where
  one [] _       = []
  one (p : ps) q = (p ++ q) : one ps q

-- ps <> qs = concatMap (\p -> one p qs) ps where
--   one :: String -> Regex -> Regex
--   one p []       = []
--   one p (q : qs) = (p ++ q) : one p qs


-- >>> chars ['D', 'E'] <> chars ['A', 'B', 'C']
-- ["DA","DB","DC","EA","EB","EC"]

empty :: Regex
empty = [""]

concatenate :: [Regex] -> Regex
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

question, star, plus :: Regex -> Regex

question r = empty <|> r

plus r = r <> star r

star r = question (plus r)

-----------------------------------------

upper, lower, space :: Regex
upper = chars ['A'..'Z']
lower = chars ['a'..'z']
space = chars [' ', '\t', '\r', '\n']

-- >>> take 59 (plus upper)
-- ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","AA","AB","AC","AD","AE","AF","AG","AH","AI","AJ","AK","AL","AM","AN","AO","AP","AQ","AR","AS","AT","AU","AV","AW","AX","AY","AZ","AAA","AAB","AAC","AAD","AAE","AAF","AAG"]

-- >>> take 150 (filter ((> 3) . length) (upper <> star lower))
-- ["Aaaa","Baaa","Caaa","Daaa","Eaaa","Faaa","Gaaa","Haaa","Iaaa","Jaaa","Kaaa","Laaa","Maaa","Naaa","Oaaa","Paaa","Qaaa","Raaa","Saaa","Taaa","Uaaa","Vaaa","Waaa","Xaaa","Yaaa","Zaaa","Abaa","Bbaa","Cbaa","Dbaa","Ebaa","Fbaa","Gbaa","Hbaa","Ibaa","Jbaa","Kbaa","Lbaa","Mbaa","Nbaa","Obaa","Pbaa","Qbaa","Rbaa","Sbaa","Tbaa","Ubaa","Vbaa","Wbaa","Xbaa","Ybaa","Zbaa","Acaa","Bcaa","Ccaa","Dcaa","Ecaa","Fcaa","Gcaa","Hcaa","Icaa","Jcaa","Kcaa","Lcaa","Mcaa","Ncaa","Ocaa","Pcaa","Qcaa","Rcaa","Scaa","Tcaa","Ucaa","Vcaa","Wcaa","Xcaa","Ycaa","Zcaa","Adaa","Bdaa","Cdaa","Ddaa","Edaa","Fdaa","Gdaa","Hdaa","Idaa","Jdaa","Kdaa","Ldaa","Mdaa","Ndaa","Odaa","Pdaa","Qdaa","Rdaa","Sdaa","Tdaa","Udaa","Vdaa","Wdaa","Xdaa","Ydaa","Zdaa","Aeaa","Beaa","Ceaa","Deaa","Eeaa","Feaa","Geaa","Heaa","Ieaa","Jeaa","Keaa","Leaa","Meaa","Neaa","Oeaa","Peaa","Qeaa","Reaa","Seaa","Teaa","Ueaa","Veaa","Weaa","Xeaa","Yeaa","Zeaa","Afaa","Bfaa","Cfaa","Dfaa","Efaa","Ffaa","Gfaa","Hfaa","Ifaa","Jfaa","Kfaa","Lfaa","Mfaa","Nfaa","Ofaa","Pfaa","Qfaa","Rfaa","Sfaa","Tfaa"]
