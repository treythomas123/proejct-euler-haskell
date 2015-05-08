-- Solution to Project Euler problem 41
-- By Trey Thomas
--
-- We shall say that an n-digit number is pandigital if it makes use of all the
-- digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
-- also prime.
--
-- What is the largest n-digit pandigital prime that exists?

import Data.List
import Data.Numbers.Primes

main = print $ head $ filter (isPrime . read) $ concat $ map (reverse . sort . permutations . (enumFromTo '1')) ['9','8'..'1']
