{-# LANGUAGE NoImplicitPrelude #-}

import GHC.Show
import GHC.Types (Char)
import GHC.IO (IO)
import qualified GHC.IO.Handle as IO (hPutChar)
import GHC.IO.StdHandles as Handle (stdout)

import Natural

undefined = undefined

-- set a number into a base of given size
toBase :: Positive -> Natural -> [Natural]
-- special case for base 1
toBase (Positive First) First = []
toBase (Positive First) (Next n) = First : (toBase (Positive First) n)
toBase base n = case divide n base of
  (First, remainder) -> [remainder]
  (quotient, remainder) -> remainder:(toBase base quotient)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

length :: [a] -> Natural
length [] = First
length (x:xs) = Next (length xs)

reverse :: [a] -> [a]
reverse xs = inner xs []
  where
    inner :: [a] -> [a] -> [a]
    inner [] result = result
    inner (x:xs) result = inner xs (x:result)

-- TODO: require non-empty base at the type level
showInBase :: [Char] -> Natural -> [Char]
showInBase b n = reverse (map (b !!) digits)
  where
    digits = (toBase base n)
    base = case length b of
      First -> undefined
      Next k -> Positive k

-- TODO: maybe result
(!!) :: [a] -> Natural -> a
[] !! _ = undefined
(x:_) !! First = x
(_:xs) !! (Next n) = xs !! n

-- TODO: extend to monoid addition
(+) :: Natural -> Natural -> Natural
First + n = n
n + First = n
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

putChar :: Char -> IO ()
putChar = IO.hPutChar Handle.stdout

putStrLn :: [Char] -> IO ()
putStrLn [] = putChar '\n'
putStrLn (c:cs) = do
  putChar c
  putStrLn cs

main :: IO ()
main = putStrLn "hello world"
