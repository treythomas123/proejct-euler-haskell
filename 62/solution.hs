-- Solution to Project Euler problem 62
-- By Trey Thomas
--
-- The cube, 41063625 (3453), can be permuted to produce two other cubes:
-- 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest cube
-- which has exactly three permutations of its digits which are also cube.
--
-- Find the smallest cube for which exactly five permutations of its digits are
-- cube.

import Data.List
import Data.Function

countPermutationsOfEach :: [Int] -> [(Int,Int)]
countPermutationsOfEach xs = [ (x, permCount (show x) (map show xs)) | x <- xs ]
    where permCount y = length . filter ((==) (sort y) . sort)

main = print $ firstWithFive $ countPermutationsInList $ cubes
    where
        cubes = map (^3) [1..]
        countPermutationsInList = concat . map countPermutationsOfEach . groupBy sameDigitCount
        sameDigitCount = (==) `on` (length . show)
        firstWithFive = fst . head . filter ((==5) . snd)
