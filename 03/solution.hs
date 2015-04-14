-- Solution to Project Euler problem 3
-- By Trey Thomas
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

primes = 2:3:[ n | n <- [5,7..], isPrime n]

isPrime n = null (primeFactors n)

primeFactors n = filter (\x -> mod n x == 0) (takeWhile (\p -> p*p <= n) primes)

main = print $ last $ primeFactors 600851475143
