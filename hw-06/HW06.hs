{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n
  | n > 0= fib (n-1) + fib (n-2)
  | otherwise = undefined

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a s) = Cons (f a) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f $ f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s1) s2 = Cons x $ sInterleave s2 s1

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x s)
  | n > 0 = x : sTake (n-1) s
  | otherwise = undefined

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) ((+1) <$> ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (fromIntegral . (`mod` 2147483648) . (+12345) . (*1103515245) . (fromIntegral :: Int -> Integer))

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = minMaxAcc x x xs
  where
  minMaxAcc :: Int -> Int -> [Int] -> Maybe (Int, Int)
  minMaxAcc !mn !mx [] = Just (mn, mx)
  minMaxAcc !mn !mx (y:yx) = minMaxAcc (min mn y) (max mx  y) yx

main :: IO ()
-- main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix a = M {a11 :: a,  a12 :: a, a21 :: a, a22 :: a}

instance Num a => Num (Matrix a) where
  (M a11 a12 a21 a22) * (M b11 b12 b21 b22) = M (a11*b11 + a12*b21) (a11*b12 + a12*b22) (a21*b11 + a22*b21) (a21*b12 + a22*b22)
  (+) = undefined
  (-) = undefined
  abs = undefined
  signum = undefined
  fromInteger x = M (fromInteger x) 0 0 (fromInteger x)

fastFib :: Int -> Integer
fastFib n = a11 $ M 1 1 1 0 ^ n
