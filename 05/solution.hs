-- Solution to Project Euler problem 5
-- By Trey Thomas
--
-- 2520 is the smallest number that can be divided by each of the numbers from 1
-- to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the
-- numbers from 1 to 20?

primes = 2:3:[ n | n <- [5,7..], isPrime n]

isPrime n = null (primeFactors n)

primeFactors n = filter (\x -> mod n x == 0) (takeWhile (\p -> p*p <= n) primes)
