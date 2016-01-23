{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x : y : zs) = x : 2 * y : doubleEveryOther zs
doubleEveryOther xs = xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toRevDigits) 0


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = (sumDigits . doubleEveryOther . toRevDigits) n  `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src dst _ = [(src, dst)]
hanoi n src dst stg = hanoi (n - 1) src stg dst ++ (src, dst) : hanoi (n - 1) stg dst src

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 1 src dst _ _ = [(src, dst)]
hanoi4 2 src dst stg1 _ = hanoi 2 src dst stg1
hanoi4 n src dst stg1 stg2 = hanoi4 (n - 2) src stg1 stg2 dst ++ hanoi 2 src dst stg2 ++ hanoi4 (n - 2) stg1 dst stg2 src
