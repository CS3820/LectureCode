{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use map once" -}
module Lecture2a where

import           Data.Char

{--

A Haskell file is a list of definitions, each of which consists of:

- An (optional) type signature
- A (non-optional) list of equations

--}

x = length message

{--

Functions aren't always written the same way:

 - f(x)
 - sin x
 - x!
 - x²

In many programming languages, function calls are written with parentheses:

  print(5);
  x = Math.sin(2 * Math.pi)

--}

{--

Equations:

Every equation has a left-hand (LHS) and a right-hand side (RHS)

 - the LHS is a *pattern* --- so far, just the name of the thing we're defining
 - the RHS is an *expressions* --- constants (5, "this"), function calls (5 + 5, head message)

--}

first = head message

{--

  first
=
  head message
=
  head "Attack at dawn"
=
  'A'

--}

{--

A *higher-order function* is a function that either

  - takes other functions as arguments
  - returns other functions as results

The prototypical higher order function is *map*

--}

bigMessage = bigger message

-- >>> bigMessage
-- "ATTACK AT DAWN"

messageCodes = map ord message

-- >>> messageCodes
-- [65,116,116,97,99,107,32,97,116,32,100,97,119,110]

{--

We CAN define functions by writing equations with "function calls" on the LHS.

--}

f :: Int -> Int
f x = x + 5

-- >>> map f messageCodes
-- [70,121,121,102,104,112,37,102,121,37,105,102,124,115]

{--

  map f messageCodes
=
  map f [65,116,116,97,99,107,32,97,116,32,100,97,119,110]
=
  [f 65,f 116,f 116,f 97,...]
=
  [65 + 5, 116 + 5, 116 + 5, 97 + 5, ...]
=
  [70, 121, 121, ...]

--}

bigger :: [Char] -> [Char]
bigger cs = map toUpper cs
-- ^-f  ^- x

{--

  bigger message
=     { x = y ==> f x = f y }
  bigger "Attack at dawn"
=     { f = g ==> f x = g x }
  map toUpper "Attack at dawn"
=     { def'n map }
  [toUpper 'A', toUpper 't', toUpper 't', ...]
=     { def'n toUpper x3 }
  ['A', 'T', 'T', ...]

--}

bigger' :: [Char] -> [Char]
bigger' = map toUpper

{--

  bigger' message
=
  bigger' "Attack at dawn"
=
  map toUpper "Attack at dawn"
=
  [toUpper 'A', toUpper 't', toUpper 't', ...]
=
  ['A', 'T', 'T', ...]

--}

{---------

We're going to implement *Caesar Cyphers* or *shift cyphers*, which a simple
encyphering mechanism supposedly used by Julius Caesar.

---------}

message :: String
message = "Attack at dawn"

shift3 :: Char -> Char
shift3 c =
  if isAlpha c
  then chr (ord c + 3)
  else c

shift3' :: Char -> Char
shift3' c
  | isAlpha c  = chr (ord c + 3)
  | isNumber c = chr (ord c + 3)
  | otherwise  = c

caesar0 :: String -> String
caesar0 cs = map shift3' cs

-- >>> caesar0 (message ++ "123")
-- "Dwwdfn dw gdzq456"

caesar1 :: Int -> String -> String
caesar1 n = map shift
  where
  shift c
    | isLower c  = (((f c) 'a') 26) -- chr ((ord c + n - ord 'a') `mod` 26 + ord 'a')
    | isUpper c  = f c 'A' 26 -- chr ((ord c + n - ord 'A') `mod` 26 + ord 'A')
    | isNumber c = f c '0' 10 -- chr ((ord c + n - ord '0') `mod` 10 + ord '0')
    | otherwise  = c
    where f :: Char -> (Char -> (Int -> Char))
          f c d m = chr ((ord c + n - ord d) `mod` m + ord d)

-- >>> (caesar1 3) (message ++ "789")
-- "Dwwdfn dw gdzq012"

-- >>> caesar1 (-4) "Exxego ex hear123"
-- "Attack at dawn789"

{--

For *integers* `x, y ∈ Z`, `y` divides `x` (syntax `y | x`) if and only if
there exists some  `k ∈ ℤ` such that `k * y = x`. Because there is *always* a
`k` such that `y * k = 0`, *every* number divides 0. In particular, 2 divides 0,
and so 0 is even.

--}
