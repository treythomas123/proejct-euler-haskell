-- Solution to Project Euler problem 53
-- By Trey Thomas
--
-- There are exactly ten ways of selecting three from five, 12345:
--
-- 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
--
-- In combinatorics, we use the notation, 5C3 = 10.
--
-- In general,
--
-- nCr =
-- n!r!(n−r)!
-- ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
--
-- It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
--
-- How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are
-- greater than one-million?

choose :: (Integral a) => a -> a -> a
choose n r = product [r+1..n] `div` product [1..n-r]

main = print $ length $ filter (>1000000) [ choose n r | n <- [1..100], r <- [1..n] ]
