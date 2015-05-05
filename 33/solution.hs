-- Solution to Project Euler problem 33
-- By Trey Thomas
--
-- The fraction 49/98 is a curious fraction, as an inexperienced mathematician
-- in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
-- is correct, is obtained by cancelling the 9s.
--
-- We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
--
-- There are exactly four non-trivial examples of this type of fraction, less
-- than one in value, and containing two digits in the numerator and
-- denominator.
--
-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.

import Data.List

curiousFractions :: Int -> Int -> Int -> [(Int,Int)]
curiousFractions n d c = [ (a,b) |
    nc <- permutations [n,c],
    dc <- permutations [d,c],
    let a = nc!!0 * 10 + nc!!1,
    let b = dc!!0 * 10 + dc!!1,
    a < b,
    a * d == b * n]

prod :: [(Int,Int)] -> (Int,Int)
prod fracs = (product $ map fst fracs, product $ map snd fracs)

lowestCommonTerms :: (Int,Int) -> (Int,Int)
lowestCommonTerms (n,d) = (div n g, div d g)
    where g = gcd n d

main = print $ snd $ lowestCommonTerms $ prod $ concat [ curiousFractions n d c | n<-[1..9], d<-[1..9], c<-[1..9]]
