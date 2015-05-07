-- Solution to Project Euler problem 40
-- By Trey Thomas
--
-- An irrational decimal fraction is created by concatenating the positive
-- integers:
--
-- 0.123456789101112131415161718192021...
--
-- It can be seen that the 12th digit of the fractional part is 1.
--
-- If d_n represents the nth digit of the fractional part, find the value of the
-- following expression.
--
-- d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000

import Data.Char

d = digitToInt . (!!) (concat $ map show [0..])

main = print $ product [d 1, d 10, d 100, d 1000, d 10000, d 100000, d 1000000]
