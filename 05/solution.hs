-- Solution to Project Euler problem 5
-- By Trey Thomas
--
-- 2520 is the smallest number that can be divided by each of the numbers from 1
-- to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

import Data.List
import Control.Arrow

primes :: [Int]
primes = 2:[ n | n <- [3,5..], isPrime n]

isPrime :: Int -> Bool
isPrime n = length (primeFactorize n) == 1

primeFactorize :: Int -> [Int]
primeFactorize 1 = []
primeFactorize n = factor : primeFactorize (div n factor)
    where possibleFactors = takeWhile (\p -> p*p <= n) primes ++ [n]
          factor = head [ x | x <- possibleFactors, mod n x == 0]

maxCount :: Eq a => a -> [(Int,a)] -> Int
maxCount x xcounts = maximum $ map fst $ filter (\e -> snd e == x) xcounts

flatten :: [[a]] -> [a]
flatten = foldl (++) []

pack :: Eq a => [a] -> [(Int,a)]
pack = map (length &&& head) . group 

unpack :: [(Int,a)] -> [a]
unpack = flatten . map (\e -> take (fst e) (repeat (snd e)))

commonPrimeFactors :: [Int] -> [Int] 
commonPrimeFactors xs = unpack [ (maxCount f eachFactorCounts, f) | f <- factors ]
    where eachFactored = map primeFactorize xs 
          eachFactorCounts = flatten $ map pack eachFactored
          factors = nub $ foldl (++) [] eachFactored

leastCommonMultiple :: [Int] -> Int
leastCommonMultiple = foldl (*) 1 . commonPrimeFactors

main = print $ leastCommonMultiple [1..20]
