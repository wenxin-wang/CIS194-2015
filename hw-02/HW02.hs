{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.Function

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches s = length . filter (== True) . zipWith (==) s

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = map (\x -> length $ filter (==x) c) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches s = sum . on (zipWith min) countColors s
-- matches s g = sum $ zipWith min (countColors s) (countColors g)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g exact noneExact
  where
    exact = exactMatches s g
    noneExact = matches s g - exact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent m@(Move g _ _) s = getMove s g == m

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (: []) colors
allCodes n
  | n <= 0 = undefined
  | otherwise = concatMap (\x -> map (x:) (allCodes $ n-1)) colors

-- Exercise 7 -----------------------------------------

alwaysFirst :: Code -> [Code] -> [Move]
alwaysFirst _ [] = []
alwaysFirst s (g:rest) = m : alwaysFirst s (filterCodes m rest)
  where
    m = getMove s g

solve :: Code -> [Move]
solve s = alwaysFirst s $ allCodes $ length s

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
