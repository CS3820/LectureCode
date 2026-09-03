{- HLINT ignore "Eta reduce" -}
{- HLINT ignore "Use map once" -}
module Lecture2aFull where

import           Data.Char

{-------------------------------------------------------------------------------

Today's motivating example is a Caesar, or shift cypher.  But we're going to get
there in several stages.

The first point we need to make is that a Haskell file is a series of
definitions.  A definition contains:

* A type signature
* A series of equations

where each equation has *at least*:

* A left-hand side---the thing we're defining.
* A right-hand side---the body of the definition.

We'll see more of the structure of definitions as we go along...

-------------------------------------------------------------------------------}

message :: String
message = "Attack at dawn"

shiftAmount :: Int
shiftAmount = 3

{-------------------------------------------------------------------------------

The right-hand side need not be a single expression---there could be computation
there.

-------------------------------------------------------------------------------}

biggerShiftAmount :: Int
biggerShiftAmount = shiftAmount + 1

{-------------------------------------------------------------------------------

(+) is actually just a Haskell function like any other... but unlike most
Haskell functions, it's (*by default*) written infix.  Most Haskell functions
are *by default* written prefix:

-------------------------------------------------------------------------------}

shiftedLetter :: Char
shiftedLetter = chr (ord 'a' + biggerShiftAmount)

{-------------------------------------------------------------------------------

We can evaluate Haskell expressions either using the Haskell interpreter `ghci`,
or by using doctest-style comments in our source code.  I'll mostly rely on the
latter for now.

-------------------------------------------------------------------------------}

-- >>> biggerShiftAmount
-- 4

-- >>> shiftedLetter
-- 'e'

{-------------------------------------------------------------------------------

We define our own functions using equations, the same way we define constants.
The left-hand side of such an equation is a *pattern*---you can think of it as
an example of the uses of the thing we're defining:

-------------------------------------------------------------------------------}

shiftConstant :: Char -> Char
shiftConstant c = chr (shiftAmount + ord c)

-- >>> shiftConstant 'a'
-- 'd'

-- >>> shiftConstant 'C'
-- 'F'

{-------------------------------------------------------------------------------

A "higher-order" function is a function that operates on other
functions---either as an argument or as a result.

As far as Haskell is concerned, a higher-order function is just like any other
function---there's no special syntax to define or use one.

Higher-order functions are completely pervasive.

A prototypical example is the `map` function: `map f xs` applies `f` to every
element of list `xs`.  (Oh, that reminds me: Haskell strings are just lists of
characters...)

-------------------------------------------------------------------------------}

caesarConstant :: String -> String
caesarConstant s = map shiftConstant s

-- >>> caesarConstant "attack"
-- "dwwdfn"

-- >>> caesarConstant "zulu"
-- "}xox"

-- >>> caesarConstant message
-- "Dwwdfn#dw#gdzq"

{-------------------------------------------------------------------------------

There are clearly some problems here.  But let's put them on hold for a minute
and talk more about functions.

Part of programming with functions is recognizing that we can decompose and
recompose functions. Let's consider the shiftConstant function above. This
function has three steps: convert to a number, add the shift amount, and then
convert back to a character. We can make this more explicit in the way we write
it.

-------------------------------------------------------------------------------}

addShiftAmount :: Int -> Int
addShiftAmount n = shiftAmount + n

shiftConstant0 :: Char -> Char
shiftConstant0 c = chr (addShiftAmount (ord c))

{-------------------------------------------------------------------------------

Haskell has a function *composition* operator, spelled `.`. It works just the
same as function composition did in high school algebra, or in discrete math:

    (f . g) x = f (g x)

Using the composition operator, we can rewrite the shiftConstant function:

-------------------------------------------------------------------------------}

shiftConstant1 :: Char -> Char
shiftConstant1 c = (chr . addShiftAmount) (ord c)

shiftConstant2 :: Char -> Char
shiftConstant2 c = (chr . addShiftAmount . ord) c


{-------------------------------------------------------------------------------

Now, notice that the left and right side of this equation are both of the form
`f c` for some `c`: on the left hand, `f` is `shiftConstant2`, on the right
hand, it's `chr . addShiftAmount . ord`. We can simplify the definition by
removing the `c` part. This is formally called an η-reduction...

-------------------------------------------------------------------------------}

shiftConstant3 :: Char -> Char
shiftConstant3 = chr . addShiftAmount . ord

{-------------------------------------------------------------------------------

Remember, we haven't changed the *behavior* of shiftConstant through these
iterations; what we've changed is how we *represent* that behavior. Now, let's
return to the `caesarConstant` function. It had a use of `shiftConstant` in it,
which we can replace by `shiftConstant3`.

-------------------------------------------------------------------------------}

caesarConstant0 :: String -> String
caesarConstant0 s = map (chr . addShiftAmount . ord) s

{-------------------------------------------------------------------------------

Now, we're going to introduce a "fusion" law:

    map (f . g) x = (map f . map g) x

That is: mapping a composition of functions is the same as a composition of
maps. Normally, we'd want to use this rule from right to left, to remove maps.
In this case, I want to use it left to right:

-------------------------------------------------------------------------------}

caesarConstant1 :: String -> String
caesarConstant1 s = (map chr . map addShiftAmount . map ord) s

-- Now, we can watch caesarConstant progress in stages

-- >>> map ord message

-- >>> (map addShiftAmount . map ord) message

-- >>> (map chr . map addShiftAmount . map ord) message


{-------------------------------------------------------------------------------

Multiple argument functions are written just the same as single argument
functions---separate arguments with spaces.

(In fact, there's really no such thing as a multiple argument function in
Haskell, but we'll talk about that later...)

-------------------------------------------------------------------------------}

shift :: Int -> Char -> Char
shift n c = chr (n + ord c)

-- >>> shift shiftAmount 'a'

-- >>> shift biggerShiftAmount 'F'


{-------------------------------------------------------------------------------


Multiple argument functions are actually higher-order functions... when we write

    shift :: Int -> Char -> Char

we're really defining a function that returns a function: `shift n` returns a
`Char -> Char` function.

This is why you can just write a bunch of arguments in row: `shift 3 'c'` is two
function applications: `(shift 3) 'c'`.

This means that we can also use `shift 3` anywhere we need a `Char -> Char`
function.

-------------------------------------------------------------------------------}

caesar0 :: Int -> String -> String
caesar0 n s = map (shift n) s

-- >>> caesar0 shiftAmount "attack"
-- "dwwdfn"

-- >>> caesar0 shiftAmount "zulu"
-- "}xox"

-- >>> caesar0 shiftAmount message
-- "Dwwdfn#dw#gdzq"

{-------------------------------------------------------------------------------

Let's make one more observation here: the argument `s` isn't doing anything.
We can remove it---this is formally called an η-reduction...

-------------------------------------------------------------------------------}

caesar0' :: Int -> String -> String
caesar0' n = map (shift n)

-- >>> caesar0' shiftAmount "attack"
-- "dwwdfn"

-- >>> caesar0' shiftAmount "zulu"
-- "}xox"

-- >>> caesar0' shiftAmount message
-- "Dwwdfn#dw#gdzq"

{-------------------------------------------------------------------------------

One problem we have here is that we're shifting non-letter characters---like
spaces.  Let's define a new version of the shift function that doesn't apply to
non-letters.  This gives us a good reason to introduce the next feature of
equations: guards.

-------------------------------------------------------------------------------}

shift' :: Int -> Char -> Char
shift' n c
  | isLetter c = shift n c
  | otherwise  = c

caesar1 :: Int -> String -> String
caesar1 n = map (shift' n)

{-------------------------------------------------------------------------------

We might want to think of the definition of the shifting function as part of the
definition of the caesar function.... so we could write it as a *local*
definition.

-------------------------------------------------------------------------------}

caesar1' :: Int -> String -> String
caesar1' n = map (shift' n)
  where shift' :: Int -> Char -> Char
        shift' n c
          | isLetter c = shift n c
          | otherwise  = c

{-------------------------------------------------------------------------------

Oh, we also have a wrap-around problem.

-------------------------------------------------------------------------------}

caesar2 :: Int -> String -> String
caesar2 n = map (shift n)
  where shift :: Int -> Char -> Char
        shift n c
          | not (isLetter c)  = c
          | toUpper c > wrap  = chr (ord c - 26 + n)
          | otherwise         = chr (ord c + n)
          where wrap :: Char
                wrap = chr (ord 'Z' - n)

-- >>> caesar2 shiftAmount message
-- "Dwwdfn dw gdzq"

{-------------------------------------------------------------------------------

Remember function composition from high-school algebra?

    (f ∘ g)(x) = f(g(x))

In Haskell, we spell that as `.`.  Here's a teaser:

-------------------------------------------------------------------------------}

caesar2' :: Int -> String -> String
caesar2' = map . shift
  where shift :: Int -> Char -> Char
        shift n c
          | not (isLetter c)  = c
          | toUpper c > wrap  = chr (ord c - 26 + n)
          | otherwise         = chr (ord c + n)
          where wrap :: Char
                wrap = chr (ord 'Z' - n)

-- >>> caesar2' shiftAmount message
-- "Dwwdfn dw gdzq"
