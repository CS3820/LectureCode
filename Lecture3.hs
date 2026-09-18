{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Redundant bracket" -}
module Lecture3 (module Lecture3) where

import           Prelude hiding (pred, reverse)

-- >>> 1 + 1
-- 2

-- Are these really *just* the natural numbers?
data Nat = Zero | Succ Nat
  deriving (Eq)

foldNat :: a -> (a -> a) -> Nat -> a
foldNat z s Zero     = z
foldNat z s (Succ n) = s (foldNat z s n)

-- >>> Succ Zero
-- Succ Zero

zero, one, two, three, four :: Nat

zero = Zero
one = Succ zero
two = Succ one
three = Succ two
four = Succ three

instance Show Nat where
  show n = show (toInt n) ++ "N"

fromInt :: Integer -> Nat
fromInt n
  | n < 0  = Succ (fromInt (n + 1))
  | n == 0 = Zero
  | n > 0  = Succ (fromInt (n - 1))


-- >>> three
-- 3N

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

toIntF :: Nat -> Int
toIntF n = foldNat 0 addOne n where
  addOne n = n + 1

-- >>> toInt four
-- 4

-- toIntF three
-- = foldNat 0 addOne (Succ (Succ (Succ Zero)))
-- = addOne (foldNat 0 addOne (Succ (Succ Zero)))
-- = addOne (addOne (foldNat 0 addOne (Succ Zero)))
-- = addOne (addOne (addOne (foldNat 0 addOne Zero)))
-- = addOne (addOne (addOne 0))

add, addF :: Nat -> Nat -> Nat
add m Zero     = m
add m (Succ n) = Succ p where
  p = add m n

addF m n = foldNat m Succ n

-- (addF three) (Succ (Succ Zero))
-- Succ (Succ three)

add' :: Nat -> Nat -> Nat
add' Zero n     = n
add' (Succ m) n = Succ p where
  p = add' m n

add'' :: Nat -> (Nat -> Nat)
add'' Zero     = id
add'' (Succ m) = Succ . add'' m

add''F m = foldNat id (Succ .) m

-- add''F (Succ (Succ Zero)) four
-- foldNat id (Succ .) (Succ (Succ Zero)) four
-- ((Succ .) (foldNat id (Succ .) (Succ Zero))) four
-- ((Succ .) ((Succ .) (foldNat id (Succ .) Zero))) four
-- ((Succ .) ((Succ .) id)) four
-- ((Succ .) (Succ . id)) four
-- ((Succ .) Succ) four
-- (Succ . Succ) four
-- (Succ (Succ four)


data List a           -- [a]
  = Nil               --   []
  | Cons a (List a)   --   a : as
  deriving Show

foldList :: b -> (a -> b -> b) -> List a -> b
foldList n c Nil         = n
foldList n c (Cons a as) = c a (foldList n c as)

sumList, sumListF :: List Int -> Int
sumList Nil         = 0
sumList (Cons a as) = a + q where
  q = sumList as

sumListF = foldList 0 (+)

-- >>> sumListF (Cons 1 (Cons 2 (Cons 3 Nil)))
-- 6

mapList, mapListF :: (a -> b) -> List a -> List b
mapList f Nil         = Nil
mapList f (Cons a as) = Cons (f a) bs where
  bs = mapList f as

mapListF f = foldList Nil cons where
  cons a bs = Cons (f a) bs

-- >>> mapListF (1 +) (Cons 2 (Cons 3 (Cons 4 Nil)))
-- Cons 3 (Cons 4 (Cons 5 Nil))

infinity :: Nat
infinity = Succ infinity

isZero :: Nat -> Bool
isZero Zero     = True
isZero (Succ n) = False

-- >>> isZero infinity
-- False

ones = 1 : ones

-- >>> head ones
-- 1

-- >>> sum (take 20 ones)
-- 20

-- >>> :t zipWith
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- >>> zipWith (+) [1,2,3] [4,5,6]
-- [5,7,9]

nats = 0 : zipWith (+) nats ones

-- >>> take 20 nats
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]

-- >>> take 200 nats
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- >>> fibs !! 5
-- 5

-- >>> take 20 fibs
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]


-- 0 : 1 :  1  :  2  :  3  :  5  : (+)
--          0  :  1  :  1  :  2  :  3  :  5
--          1  :  1  :  2  :  3  :  5
