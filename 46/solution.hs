-- Solution to Project Euler problem 46
-- By Trey Thomas
--
-- It was proposed by Christian Goldbach that every odd composite number can be
-- written as the sum of a prime and twice a square.
--
-- 9 = 7 + 2×12
-- 15 = 7 + 2×22
-- 21 = 3 + 2×32
-- 25 = 7 + 2×32
-- 27 = 19 + 2×22
-- 33 = 31 + 2×12
--
-- It turns out that the conjecture was false.
--
-- What is the smallest odd composite that cannot be written as the sum of a
-- prime and twice a square?

import Data.Numbers.Primes

canBeWrittenAsTheSumOfAPrimeAndTwiceASquare n = elem n [ p + 2 * s^2 | 
                                                          p <- takeWhile (<n) primes,
                                                          s <- takeWhile (\s -> 2 * s^2 < n) [1..] ]

main = print $ head $ filter (not . canBeWrittenAsTheSumOfAPrimeAndTwiceASquare) oddComposites
    where
        composites = filter (not . isPrime) [9..]
        oddComposites = filter odd composites
