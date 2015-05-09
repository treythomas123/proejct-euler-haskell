-- Solution to Project Euler problem 51
-- By Trey Thomas
--
-- By replacing the 1st digit of the 2-digit number *3, it turns out that six of
-- the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
--
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this
-- 5-digit number is the first example having seven primes among the ten
-- generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
-- 56773, and 56993. Consequently 56003, being the first member of this family,
-- is the smallest prime with this property.
--
-- Find the smallest prime which, by replacing part of the number (not
-- necessarily adjacent digits) with the same digit, is part of an eight prime
-- value family.

import Data.Numbers.Primes
import Data.List

replaceWhere :: (a -> Bool) -> a -> [a] -> [a]
replaceWhere test r [] = []
replaceWhere test r (x:xs) = replaceFirst : replaceWhere test r xs
    where replaceFirst
            | test x = r
            | otherwise = x

family s = filter ((==length s) . length . show) $ filter isPrime [ (read::String->Int) $ replaceWhere (=='*') n s | n <- ['0'..'9'] ]

patterns = concat $ [ permutations (show n ++ (take w $ repeat '*')) | n <- [1..], w <- [1..3] ]

main = print $ head $ head $ filter ((>=8) . length) $ map family patterns
