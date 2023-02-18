{-# LANGUAGE NoImplicitPrelude #-}

import GHC.Show
import GHC.Types (Char)

undefined = undefined

data Natural = Zero | Next Natural

difference :: Natural -> Natural -> (Natural, Natural)
difference Zero n = (Zero, n)
difference n Zero = (n, Zero)
difference (Next n) (Next m) = difference n m

-- division by repeated subtraction
-- the simplest form of Euclidean division
-- TODO: require positive number as divisor
divide :: Natural -> Natural -> (Natural, Natural)
divide a b = case difference a b of
  (n, Zero) -> let (quotient, rest) = divide n b in (Next quotient, rest)
  (Zero, n) -> (Zero, a)

-- set a number into a base of given size
-- TODO: require positive number as base size
toBase :: Natural -> Natural -> [Natural]
toBase Zero _ = undefined
-- special case for base 1
toBase (Next Zero) Zero = []
toBase (Next Zero) (Next n) = Zero : (toBase (Next Zero) n)
toBase base n = case divide n base of
  (Zero, remainder) -> [remainder]
  (quotient, remainder) -> remainder:(toBase base quotient)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

length :: [a] -> Natural
length [] = Zero
length (x:xs) = Next (length xs)

reverse :: [a] -> [a]
reverse xs = inner xs []
  where
    inner :: [a] -> [a] -> [a]
    inner [] result = result
    inner (x:xs) result = inner xs (x:result)

-- TODO: require non-empty base
showInBase :: [Char] -> Natural -> [Char]
showInBase b n = reverse (map (b !!) digits)
  where
    digits = (toBase (length b) n)

-- TODO: maybe result
(!!) :: [a] -> Natural -> a
[] !! _ = undefined
(x:_) !! Zero = x
(_:xs) !! (Next n) = xs !! n

-- TODO: extend to monoid addition
(+) :: Natural -> Natural -> Natural
Zero + n = n
n + Zero = n
n + (Next m) = (Next n) + m

arabic = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
binary = [ 'O', 'I' ]
unary = [ '|' ]

instance Show Natural where
  show = showInBase arabic

newtype Unary = Unary Natural

instance Show Unary where
  show (Unary n) = showInBase unary n

newtype Binary = Binary Natural

instance Show Binary where
  show (Binary n) = showInBase binary n

newtype Arabic = Arabic Natural

instance Show Arabic where
  show (Arabic n) = showInBase arabic n

