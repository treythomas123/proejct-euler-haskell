-- Solution to Project Euler problem 49
-- By Trey Thomas
--
-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
-- increases by 3330, is unusual in two ways: (i) each of the three terms are
-- prime, and, (ii) each of the 4-digit numbers are permutations of one another.
--
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
-- exhibiting this property, but there is one other 4-digit increasing sequence.
--
-- What 12-digit number do you form by concatenating the three terms in this
-- sequence?

import Data.Numbers.Primes
import Data.List

fourDigitPrimes :: [Int]
fourDigitPrimes = takeWhileFourDigits $ dropWhileUnderFourDigits primes
    where
        dropWhileUnderFourDigits = dropWhile ((<4) . length . show)
        takeWhileFourDigits = takeWhile ((==4) . length . show)

permsInPrimeList :: Int -> [Int]
permsInPrimeList = (map read) . sort . nub . filter (\n -> elem (read n) fourDigitPrimes) . (permutations . show)

isArithmetic :: [Int] -> Bool
isArithmetic (x:y:z:rest) = (y-x) == (z-y) && isArithmetic (y:z:rest)
isArithmetic _ = True

main = putStrLn $ concat $ map show $ head $ notStartingWith1487 $ arithmeticSequencesOfFourDigitPrimes
    where 
        notStartingWith1487 = filter ((/=1487) . head)
        arithmeticSequencesOfFourDigitPrimes =
            filter isArithmetic
            $ concat $ map (filter ((==3) . length))
            $ map subsequences $ nub
            $ map permsInPrimeList fourDigitPrimes

