-- Solution to Project Euler problem 54
-- By Trey Thomas
--
-- In the card game poker, a hand consists of five cards and are ranked, from
-- lowest to highest, in the following way:
--
-- High Card: Highest value card.
-- One Pair: Two cards of the same value.
-- Two Pairs: Two different pairs.
-- Three of a Kind: Three cards of the same value.
-- Straight: All cards are consecutive values.
-- Flush: All cards of the same suit.
-- Full House: Three of a kind and a pair.
-- Four of a Kind: Four cards of the same value.
-- Straight Flush: All cards are consecutive values of same suit.
-- Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
--
-- The cards are valued in the order:
-- 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
--
-- If two players have the same ranked hands then the rank made up of the
-- highest value wins; for example, a pair of eights beats a pair of fives (see
-- example 1 below). But if two ranks tie, for example, both players have a pair
-- of queens, then highest cards in each hand are compared (see example 4
-- below); if the highest cards tie then the next highest cards are compared,
-- and so on.
--
-- Consider the following five hands dealt to two players:
--
-- Hand Player 1      Player 2            Winner
-- 1 5H 5C 6S 7S KD   2C 3S 8S 8D TD      Player 2
-- Pair of Fives      Pair of Eights
--
-- 2 5D 8C 9S JS AC   2C 5C 7D 8S QH      Player 1
-- Highest card Ace   Highest card Queen
--
-- 3 2D 9C AS AH AC   3D 6D 7D TD QD      Player 2
-- Three Aces         Flush with Diamonds
--
-- 4 4D 6S 9H QH QC                 3D 6D 7H QD QS                    Player 1
-- Pair of Queen Highest card Nine  Pair of QueensHighest card Seven 
--
--
-- 5 2H 2D 4C 4D 4S            3C 3D 3S 9S 9D
-- Full HouseWith Three Fours  Full Housewith Three Threes Player 1
--
-- The file, poker.txt, contains one-thousand random hands dealt to two players.
-- Each line of the file contains ten cards (separated by a single space): the
-- first five are Player 1's cards and the last five are Player 2's cards. You
-- can assume that all hands are valid (no invalid characters or repeated
-- cards), each player's hand is in no specific order, and in each hand there is
-- a clear winner.
--
-- How many hands does Player 1 win?

import Data.Char
import Data.List


allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)


data Card = Card Int Char deriving Show
suitOf (Card _ s) = s
valueOf (Card v _) = v

instance Eq Card where
    (==) (Card v1 s1) (Card v2 s2) = (v1 == v2)

data Hand = Hand [Card]

instance Eq Hand where
    (==) (Hand h1) (Hand h2) = (h1 == h2)

instance Show Hand where
    show (Hand cards) = unwords $ map (\ (Card v s) -> show v ++ [s]) cards


parseGame :: String -> (Hand,Hand)
parseGame cardsString = (h1,h2)
    where
        h1 = parseHand $ take 14 cardsString
        h2 = parseHand $ take 14 $ drop 15 cardsString

parseHand :: String -> Hand
parseHand = Hand . (map parseCard) . words

parseCard :: String -> Card
parseCard [v,suit] = Card value suit
    where value
            | v == 'T' = 10
            | v == 'J' = 11
            | v == 'Q' = 12
            | v == 'K' = 13
            | v == 'A' = 14
            | otherwise = digitToInt v

winner :: Hand -> Hand -> Hand
winner h1 h2
    | h1rank < h2rank = h1
    | h1rank > h2rank = h2
    | firstHighCardDifference > 0 = h1
    | otherwise = h2
    where
        h1rank = fst $ rank h1
        h2rank = fst $ rank h2
        h1highCards = snd $ rank h1
        h2highCards = snd $ rank h2
        highCardDiffs = zipWith (-) h1highCards h2highCards
        firstHighCardDifference = head $ dropWhile (==0) highCardDiffs


rank :: Hand -> (Int,[Int])
rank (Hand cards)
    -- Straight Flush: All cards are consecutive values of same suit.
    | isStraight && isFlush
        = (1, [last cardValues])

    -- Four of a Kind: Four cards of the same value.
    | allTheSame (init cardValues)
        = (2, [head cardValues, last cardValues])
    | allTheSame (tail cardValues)
        = (2, [last cardValues, head cardValues])

    -- Full House: Three of a kind and a pair.
    | allTheSame (take 3 cardValues) && allTheSame (take 2 $ drop 3 cardValues)
        = (3, [cardValues!!0, cardValues!!3])
    | allTheSame (take 2 cardValues) && allTheSame (take 3 $ drop 2 cardValues)
        = (3, [cardValues!!2, cardValues!!0])

    -- Flush: All cards of the same suit.
    | isFlush
        = (4, reverse cardValues)

    -- Straight: All cards are consecutive values.
    | isStraight
        = (5, reverse cardValues)

    -- Three of a Kind: Three cards of the same value.
    | allTheSame (take 3 cardValues)
        = (6, [cardValues!!0, cardValues!!4, cardValues!!3])
    | allTheSame (take 3 $ drop 1 cardValues)
        = (6, [cardValues!!1, cardValues!!4, cardValues!!0])
    | allTheSame (take 3 $ drop 2 cardValues)
        = (6, [cardValues!!3, cardValues!!1, cardValues!!0])

    -- Two Pairs: Two different pairs.
    | allTheSame (take 2 cardValues) && allTheSame (take 2 $ drop 2 cardValues)
        = (7, [max (cardValues!!0) (cardValues!!2), min (cardValues!!0) (cardValues!!2), cardValues!!4])
    | allTheSame (take 2 cardValues) && allTheSame (take 2 $ drop 3 cardValues)
        = (7, [max (cardValues!!0) (cardValues!!3), min (cardValues!!0) (cardValues!!3), cardValues!!2])
    | allTheSame (take 2 $ drop 1 cardValues) && allTheSame (take 2 $ drop 3 cardValues)
        = (7, [max (cardValues!!1) (cardValues!!3), min (cardValues!!1) (cardValues!!3), cardValues!!0])

    -- One Pair: Two cards of the same value.
    | (length $ nub cardValues) == 4
        = let pairVal = head $ cardValues \\ (nub cardValues) in
          (8, pairVal : (reverse $ filter (/=pairVal) cardValues))

    -- High Card: Highest value card.
    | otherwise = (9, reverse cardValues)
    where
        cardValues = sort $ map valueOf cards
        cardSuits = map suitOf cards
        isStraight = elem cardValues [ [n..n+4] | n <- [2..11] ]
        isFlush = allTheSame cardSuits


main = do
    fileContents <- readFile "poker.txt"
    let games = map parseGame $ lines $ fileContents
    print $ length $ filter (\ (h1,h2) -> winner h1 h2 == h1) games
