-- Solution to Project Euler problem 48
-- By Trey Thomas
--
-- The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
--
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

lastN n xs = drop (length xs - n) xs

main = putStrLn $ lastN 10 $ show $ sum $ [ n^n | n <- [1..1000] ]
