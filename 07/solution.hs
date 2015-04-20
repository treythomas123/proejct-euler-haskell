-- Solution to Project Euler problem 7
-- By Trey Thomas
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
-- that the 6th prime is 13.
--
-- What is the 10 001st prime number?

primes :: [Int]
primes = 2:[ n | n <- [3,5..], isPrime n]

isPrime :: Int -> Bool
isPrime n = length (primeFactorize n) == 1

primeFactorize :: Int -> [Int]
primeFactorize 1 = []
primeFactorize n = factor : primeFactorize (div n factor)
    where possibleFactors = takeWhile (\p -> p*p <= n) primes ++ [n]
          factor = head [ x | x <- possibleFactors, mod n x == 0]

main = print $ primes !! 10000
