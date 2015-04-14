-- Solution to Project Euler problem 4
-- By Trey Thomas
--
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome s = s == reverse s

threeDigitNumbers = [100..999]

productsOfThreeDigitNumbers = [ x * y | x <- threeDigitNumbers, y <- threeDigitNumbers, x <= y ]

main = print $ maximum $ filter (\p -> isPalindrome (show p)) productsOfThreeDigitNumbers
