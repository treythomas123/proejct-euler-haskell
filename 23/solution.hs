-- Solution to Project Euler problem 23
-- By Trey Thomas
--
-- A perfect number is a number for which the sum of its proper divisors is
-- exactly equal to the number. For example, the sum of the proper divisors of
-- 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
--
-- A number n is called deficient if the sum of its proper divisors is less than
-- n and it is called abundant if this sum exceeds n.
--
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
-- number that can be written as the sum of two abundant numbers is 24. By
-- mathematical analysis, it can be shown that all integers greater than 28123
-- can be written as the sum of two abundant numbers. However, this upper limit
-- cannot be reduced any further by analysis even though it is known that the
-- greatest number that cannot be expressed as the sum of two abundant numbers
-- is less than this limit.
--
-- Find the sum of all the positive integers which cannot be written as the sum
-- of two abundant numbers.

import Data.List
import qualified Data.Set as Set

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

abundants = Set.fromList $ filter isAbundant [1..28123]
    where isAbundant n = n < sum (properDivisors n)

isSumOfTwoAbundants x = any (\a -> Set.member (x-a) abundants) $ Set.elems abundants

main = print $ sum $ filter (not . isSumOfTwoAbundants) $ [1..28123]
