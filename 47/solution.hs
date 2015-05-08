-- Solution to Project Euler problem 47
-- By Trey Thomas
--
-- The first two consecutive numbers to have two distinct prime factors are:
--
-- 14 = 2 × 7
-- 15 = 3 × 5
--
-- The first three consecutive numbers to have three distinct prime factors are:
--
-- 644 = 2² × 7 × 23
-- 645 = 3 × 5 × 43
-- 646 = 2 × 17 × 19.
--
-- Find the first four consecutive integers to have four distinct prime factors.
-- What is the first of these numbers?

import Data.Numbers.Primes
import Data.List
import Data.Maybe

main = print $ head $ fromJust $ find allHaveFourDistinctPrimeFactors $ map (take 4 . enumFrom) [1..]
    where allHaveFourDistinctPrimeFactors = all $ (>=4) . length . nub . primeFactors
