-- Solution to Project Euler problem 36
-- By Trey Thomas
--
-- The decimal number, 585 = 1001001001₂ (binary), is palindromic in both bases.
--
-- Find the sum of all numbers, less than one million, which are palindromic in
-- base 10 and base 2.
--
-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

import Numeric
import Data.Char

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

showBinary :: Int -> String
showBinary n = showIntAtBase 2 intToDigit n ""

main = print $ sum $ filter doublePalindrome [1..999999]
    where
        doublePalindrome n = (isPalindrome $ showBinary n) && (isPalindrome $ show n)
