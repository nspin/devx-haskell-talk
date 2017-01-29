fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

seive :: [Integer] -> [Integer]
seive (p:xs) = p : seive [ x | x <- xs, x `mod` p /= 0 ]

primes :: [Integer]
primes = seive [2..]
