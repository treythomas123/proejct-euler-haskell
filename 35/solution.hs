-- Solution to Project Euler problem 35
-- By Trey Thomas
--
-- The number, 197, is called a circular prime because all rotations of the
-- digits: 197, 971, and 719, are themselves prime.
--
-- There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
-- 73, 79, and 97.
--
-- How many circular primes are there below one million?


import Data.List

primes :: [Int]
primes = 2:[ n | n <- [3,5..], isPrime n]

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = length (primeFactorize n) == 1

primeFactorize :: Int -> [Int]
primeFactorize 0 = []
primeFactorize 1 = []
primeFactorize n = factor : primeFactorize (div n factor)
    where possibleFactors = takeWhile (\p -> p*p <= n) primes ++ [n]
          factor = head [ x | x <- possibleFactors, mod n x == 0]


rotate :: [a] -> Int -> [a]
rotate xs n = take l $ drop (mod n l) $ cycle xs
    where l = length xs

isCircularPrime :: Int -> Bool
isCircularPrime n = all isPrime $ rotations
    where 
        nStr = show n
        rotations = [ (read::String->Int) $ rotate nStr r | r <- [1..length nStr - 1] ]

main = print $ length $ filter isCircularPrime $ takeWhile (<1000000) primes
