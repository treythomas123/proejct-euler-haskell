-- Solution to Project Euler problem 39
-- By Trey Thomas
--
-- If p is the perimeter of a right angle triangle with integral length sides,
-- {a,b,c}, there are exactly three solutions for p = 120.
--
-- {20,48,52}, {24,45,51}, {30,40,50}
--
-- For which value of p ≤ 1000, is the number of solutions maximised?

import Data.List
import Data.Ord

rightTrianglesWithPerimeter :: Int -> [(Int,Int,Int)]
rightTrianglesWithPerimeter p = [ (a,b,c) | 
                                    a <- [1..div p 2],
                                    let b = (p * (p - 2*a)) `div` (2*(p - a)),
                                    let c = p - a - b,
                                    a^2 + b^2 == c^2]

main = print $ maximumBy (comparing $ length . rightTrianglesWithPerimeter) [1..1000]
