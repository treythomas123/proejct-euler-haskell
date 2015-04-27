-- Solution to Project Euler problem 17
-- By Trey Thomas
--
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five,
-- then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
--
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out
-- in words, how many letters would be used?
--
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
-- forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
-- letters. The use of "and" when writing out numbers is in compliance with
-- British usage.

import Data.Char
import Data.List

wordsForNumber :: Int -> [Char]
wordsForNumber n
        | n < 20 = underTwentyNames !! n
        | n < 100 = tensNames !! (div n 10) ++ remaining 10 "-"
        | n < 1000 = wordsForNumber (div n 100) ++ " hundred" ++ remaining 100 " and "
        | otherwise = wordsForNumber (div n (1000^thousands)) ++ " " ++
                      thousandsNames !! thousands ++
                      remaining (1000^thousands) " "
    where
        thousands = head $ dropWhile (\p -> n >= 1000^(p+1)) [0..]

        remaining m delim
            | mod n m == 0  = ""
            | otherwise     = delim ++ wordsForNumber (mod n m)

        underTwentyNames = ["zero","one","two","three","four","five","six",
                            "seven","eight","nine","ten","eleven","twelve",
                            "thirteen","fourteen","fifteen","sixteen",
                            "seventeen","eighteen","nineteen"]

        tensNames = ["","","twenty","thirty","forty","fifty","sixty","seventy",
                     "eighty","ninety"]

        thousandsNames = ["","thousand","million","billion","trillion"]

main = print $ sum $ map letterCount $ map wordsForNumber [1..1000]
    where
        letterCount = length . filter isLetter
