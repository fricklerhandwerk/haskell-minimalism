{-# LANGUAGE NoImplicitPrelude #-}

module Natural where

data Natural = First | Next Natural

data Positive = Positive Natural

add :: Natural -> Natural -> Natural
add First n = n
add n First = n
add n (Next m) = add (Next n) m

-- difference between two numbers, indicating which one is larger
difference :: Natural -> Natural -> (Natural, Natural)
difference First n = (First, n)
difference n First = (n, First)
difference (Next n) (Next m) = difference n m

-- division by repeated subtraction
-- the simplest form of Euclidean division
divide :: Natural -> Positive -> (Natural, Natural)
divide a (Positive b) = case difference a (Next b) of
  (n, First) -> let (quotient, rest) = divide n (Positive b) in (Next quotient, rest)
  (First, n) -> (First, a)
