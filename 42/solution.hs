-- Solution to Project Euler problem 42
-- By Trey Thomas
--
-- The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
-- so the first ten triangle numbers are:
--
-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
--
-- By converting each letter in a word to a number corresponding to its
-- alphabetical position and adding these values we form a word value. For
-- example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value
-- is a triangle number then we shall call the word a triangle word.
--
-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text file
-- containing nearly two-thousand common English words, how many are triangle
-- words?

import Data.List
import Data.Maybe

triangleNumbers :: [Int]
triangleNumbers = [ n * (n + 1) `div` 2 | n <- [1..] ]

isTriangleNumber :: Int -> Bool
isTriangleNumber t = t == (head $ dropWhile (<t) triangleNumbers)

isTriangleWord :: String -> Bool
isTriangleWord w = isTriangleNumber wordValue
    where
        letterValue l = fromJust $ findIndex (==l) (' ':['A'..'Z'])
        wordValue = sum $ map letterValue w

parseWords :: String -> [String]
parseWords "" = []
parseWords s = unquote elem : parseWords rest
    where
        elem = takeWhile (/= ',') s
        rest = drop 1 $ dropWhile (/= ',') s
        unquote = init . tail

main = do
    fileContents <- readFile "words.txt"
    let words = parseWords fileContents
    print $ length $ filter isTriangleWord words
