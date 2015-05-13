-- Solution to Project Euler problem 52
-- By Trey Thomas
--
-- It can be seen that the number, 125874, and its double, 251748, contain
-- exactly the same digits, but in a different order.
--
-- Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
-- contain the same digits.

import Data.List

meetsCriteria x = all hasSameDigitsAsX $ map (*x) [2..6]
    where hasSameDigitsAsX n = (sort $ show n) == (sort $ show x)

main = print $ head $ filter meetsCriteria [1..]
