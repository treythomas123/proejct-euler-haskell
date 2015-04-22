-- Solution to Project Euler problem 10
-- By Trey Thomas
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.


primes :: [Int]
primes = 2:[ n | n <- [3,5..], isPrime n]

isPrime :: Int -> Bool
isPrime n = length (primeFactorize n) == 1

primeFactorize :: Int -> [Int]
primeFactorize 1 = []
primeFactorize n = factor : primeFactorize (div n factor)
    where possibleFactors = takeWhile (\p -> p*p <= n) primes ++ [n]
          factor = head [ x | x <- possibleFactors, mod n x == 0]

main = print $ sum $ takeWhile (<2000000) primes
