-- Solution to Project Euler problem 50
-- By Trey Thomas
--
-- The prime 41, can be written as the sum of six consecutive primes:
--
-- 41 = 2 + 3 + 5 + 7 + 11 + 13
--
-- This is the longest sum of consecutive primes that adds to a prime below
-- one-hundred.
--
-- The longest sum of consecutive primes below one-thousand that adds to a
-- prime, contains 21 terms, and is equal to 953.
--
-- Which prime, below one-million, can be written as the sum of the most
-- consecutive primes?

import Data.Numbers.Primes
import Data.Ord
import Data.List

primeSums = scanl1 (+) primes

solution n = head $ maximumBy (comparing length)
            $ map (dropWhile (not . isPrime))
            $ takeWhile ((>0) . length) 
                [ filter (<n) $ takeWhile (> 0) $ map (flip (-) d) $ reverse $ take 550 primeSums | d <- (0:primeSums) ]

main = print $ solution 1000000
