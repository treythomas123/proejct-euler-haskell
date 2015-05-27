-- Solution to Project Euler problem 63
-- By Trey Thomas
--
-- The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit
-- number, 134217728=89, is a ninth power.
--
-- How many n-digit positive integers exist which are also an nth power?


powerfulDigitCounts n = takeWhile ((== n) . digits) $ dropWhile ((< n) . digits) $ map (^n) [1..]
    where digits = length . show

main = print $ length $ concat $ takeWhile (not . null) $ map powerfulDigitCounts [1..]
