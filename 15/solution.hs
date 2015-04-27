-- Solution to Project Euler problem 15
-- By Trey Thomas
--
-- Starting in the top left corner of a 2×2 grid, and only being able to move to
-- the right and down, there are exactly 6 routes to the bottom right corner.
--
-- How many such routes are there through a 20×20 grid?

-- binomial coefficient function, but with n = k*2
paths :: Integer -> Integer
paths k = product [k+1..2*k] `div` product [1..k]

main = print $ paths 20
