-- Solution to Project Euler problem 22
-- By Trey Thomas
--
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
-- containing over five-thousand first names, begin by sorting it into
-- alphabetical order. Then working out the alphabetical value for each name,
-- multiply this value by its alphabetical position in the list to obtain a name
-- score.
--
-- For example, when the list is sorted into alphabetical order, COLIN, which is
-- worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.
--
-- What is the total of all the name scores in the file?

import Data.List
import Data.Maybe

parseNames :: String -> [String]
parseNames "" = []
parseNames s = unquote elem : parseNames rest
    where
        elem = takeWhile (/= ',') s
        rest = drop 1 $ dropWhile (/= ',') s
        unquote = init . tail

score :: [String] -> String -> Int
score ns n = position * alphabeticalValue n
    where
        position = 1 + (fromJust $ elemIndex n ns)
        alphabeticalValue = sum . map (\ l -> 1 + (fromJust $ elemIndex l ['A'..'Z']))

main = do
    fileContents <- readFile "names.txt"
    let sortedNames = sort $ parseNames $ fileContents
    print $ sum $ map (score sortedNames) $ sortedNames
