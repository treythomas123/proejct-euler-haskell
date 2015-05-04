-- Solution to Project Euler problem 31
-- By Trey Thomas
--
-- In England the currency is made up of pound, £, and pence, p, and there are
-- eight coins in general circulation:
--
-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
--
-- It is possible to make £2 in the following way:
--
-- 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
--
-- How many different ways can £2 be made using any number of coins?


waysToMakeChange :: Int -> [Int] -> Int
waysToMakeChange 0 _ = 1
waysToMakeChange x coins = sum $ map waysStartingWith $ possibleFirstCoins
    where
        possibleFirstCoins = filter (<=x) coins
        waysStartingWith firstCoin = waysToMakeChange remaining smallerCoins
            where
                remaining = x - firstCoin
                smallerCoins = filter (<= firstCoin) coins

main = print $ waysToMakeChange 200 [1,2,5,10,20,50,100,200]
