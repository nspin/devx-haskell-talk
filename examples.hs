import Control.Concurrent.Async
import Network.Wreq
import Control.Monad.Trans.State
import Control.Monad
import Data.Char
import Data.Bits


-- Laziness

fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

seive :: [Integer] -> [Integer]
seive (p:xs) = p : seive [ x | x <- xs, x `mod` p /= 0 ]

primes :: [Integer]
primes = seive [2..]


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
