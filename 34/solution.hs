-- Solution to Project Euler problem 34
-- By Trey Thomas
--
-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
--
-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.
--
-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

import Data.Char

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

isCurious :: Int -> Bool
isCurious n = (==n) $ sum $ map (fact . digitToInt) (show n)

main = print $ sum $ filter isCurious [10..100000]
