-- Solution to Project Euler problem 26
-- By Trey Thomas
--
-- A unit fraction contains 1 in the numerator. The decimal representation of
-- the unit fractions with denominators 2 to 10 are given:
--
-- 1/2= 0.5
--
-- 1/3= 0.(3)
--
-- 1/4= 0.25
--
-- 1/5= 0.2
--
-- 1/6= 0.1(6)
--
-- 1/7= 0.(142857)
--
-- 1/8= 0.125
--
-- 1/9= 0.(1)
--
-- 1/10= 0.1
--
-- Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
-- seen that 1/7 has a 6-digit recurring cycle.
--
-- Find the value of d < 1000 for which 1/d contains the longest recurring cycle
-- in its decimal fraction part.

import Data.List
import Data.Maybe
import Data.Char
import Control.Arrow
import Data.Ord

data DecimalFraction = DecimalFrac Int Int String

instance Show DecimalFraction where
    show (DecimalFrac bc cs cy)
        | cs > 0 = show (div bc (10^cs)) ++ "." ++ show (mod bc (10^cs)) ++ cyRep
        | otherwise = show bc ++ "." ++ cyRep
        where cyRep
                | cy == "0" = ""
                | otherwise = "(" ++ cy ++ ")"

cycleOf :: DecimalFraction -> String
cycleOf (DecimalFrac _ _ cy) = cy

decimalFrac :: Int -> Int -> DecimalFraction
decimalFrac n d = DecimalFrac beforeCycle cycleStart cycle
    where
        remainders = mod n d : [ mod (10 * remainders !! i) d | i <- [0..] ]
        digits = div n d : [ div (10 * remainders !! i) d | i <- [0..] ]
        firstThatRepeats l = fromJust $ fromJust $ find isJust [ elemIndex (l !! i) $ take i l | i <- [0..] ]
        cycleStart = firstThatRepeats remainders
        cycleLength = (+) 1 $ fromJust $ findIndex (== (remainders !! cycleStart)) $ drop (cycleStart + 1) remainders
        beforeCycle = div (n * 10^cycleStart) d
        cycle = map intToDigit $ take cycleLength $ drop (cycleStart + 1) digits

main = print $ fst $ maximumBy (comparing $ length . cycleOf . snd) $ map (id &&& decimalFrac 1) [1..999]
