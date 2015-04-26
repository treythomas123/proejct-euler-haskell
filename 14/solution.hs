-- Solution to Project Euler problem 14
-- By Trey Thomas
--
-- The following iterative sequence is defined for the set of positive integers:
--
-- n → n/2 (n is even)
-- n → 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following
-- sequence:
--
-- 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1)
-- contains 10 terms. Although it has not been proved yet (Collatz Problem), it
-- is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?
--
-- NOTE: Once the chain starts the terms are allowed to go above one million.

import Control.Arrow
import Data.List
import Data.Ord

collatz :: Int -> [Int]
collatz 0 = []
collatz n = n : collatz next
    where next
            | n == 1 = 0
            | even n = div n 2
            | odd n  = 3 * n + 1

main = print $ fst $ maximumBy (comparing snd) $ map (id &&& length . collatz) [1,3..999999]
