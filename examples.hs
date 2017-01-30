import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.Char


-- Laziness

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primes :: [Integer]
primes = sieve [2..]


-- State

type Random a = State Int a

randInt :: Random Int
randInt = state $ \g ->
    let r = a * g + c 
        a = 1103515245
        c = 12345
    in (r .&. 0x4FFFFFFF, r .&. 0x1FFFFFFF)

randLetter :: Random Char
randLetter = fmap toAlph randInt
  where toAlph i = chr (mod i 26 + ord 'a')

randPassword :: Int -> Random String
randPassword n = replicateM n randLetter
