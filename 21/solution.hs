-- Solution to Project Euler problem 21
-- By Trey Thomas
--
-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n).
-- If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
-- each of a and b are called amicable numbers.
--
-- For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
-- 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
-- 71 and 142; so d(284) = 220.
--
-- Evaluate the sum of all the amicable numbers under 10000.

import Data.List

primes :: [Int]
primes = 2:[ n | n <- [3,5..], isPrime n]

isPrime :: Int -> Bool
isPrime n = length (primeFactorize n) == 1

primeFactorize :: Int -> [Int]
primeFactorize 0 = []
primeFactorize 1 = []
primeFactorize n = factor : primeFactorize (div n factor)
    where possibleFactors = takeWhile (\p -> p*p <= n) primes ++ [n]
          factor = head [ x | x <- possibleFactors, mod n x == 0]

properDivisors :: Int -> [Int]
properDivisors = init . nub . map (foldl (*) 1) . subsequences . primeFactorize

d :: Int -> Int
d = sum . properDivisors

amicable :: Int -> Bool
amicable n = d n /= n && d (d n) == n

main = print $ sum $ filter amicable [1..9999]
