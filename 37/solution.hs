-- Solution to Project Euler problem 37
-- By Trey Thomas
--
-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain prime
-- at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
-- left: 3797, 379, 37, and 3.
--
-- Find the sum of the only eleven primes that are both truncatable from left to
-- right and right to left.
--
-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.


import Data.List

primes :: [Int]
primes = 2:[ n | n <- [3,5..], isPrime n]

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = length (primeFactorize n) == 1

primeFactorize :: Int -> [Int]
primeFactorize 0 = []
primeFactorize 1 = []
primeFactorize n = factor : primeFactorize (div n factor)
    where possibleFactors = takeWhile (\p -> p*p <= n) primes ++ [n]
          factor = head [ x | x <- possibleFactors, mod n x == 0]

truncatablePrime :: Int -> Bool
truncatablePrime n
    | length nStr < 2 = False
    | otherwise = all isPrime (leftTruncations ++ rightTruncations)
    where
        nStr = show n
        leftTruncations = [ read $ drop d $ nStr | d <- [1..length nStr-1] ]
        rightTruncations = [ read $ take t $ nStr | t <- [1..length nStr-1] ]

main = print $ sum $ take 11 $ filter truncatablePrime primes
