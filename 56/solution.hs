-- Solution to Project Euler problem 56
-- By Trey Thomas
--
-- A googol (10^100) is a massive number: one followed by one-hundred zeros;
-- 100^100 is almost unimaginably large: one followed by two-hundred zeros.
-- Despite their size, the sum of the digits in each number is only 1.
--
-- Considering natural numbers of the form, a^b, where a, b < 100, what is the
-- maximum digital sum?

import Data.Char

digitalSum :: Integer -> Int
digitalSum = sum . (map digitToInt) . show

main = print $ maximum $ [ digitalSum (a^b) | a<-[0..100], b<-[0..100] ]
