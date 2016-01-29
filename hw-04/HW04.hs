{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  -- (P la) == (P lb) = on (==) striP la lb
  (P la) == (P lb) = pEq la lb

pEq :: (Num a, Eq a) => [a] -> [a] -> Bool
pEq [] [] = True
pEq [] lb = all (== 0) lb
pEq la [] = all (== 0) la
pEq (a:la) (b:lb) = a == b && pEq la lb

striP :: (Num a, Eq a) => [a] -> [a]
striP [] = []
striP orig@(a:as)
  | a == 0 && null pas = []
  | otherwise = orig
  where pas = striP as

-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P l) = intercalate " + " (stringP l)

stringP :: (Num a, Eq a, Show a) => [a] -> [String]
stringP l = stringIP 0 l []

stringIP :: (Num a, Eq a, Show a) => Int -> [a] -> [String] -> [String]
stringIP _ [] acc = acc
stringIP n (0:zs) acc = stringIP (n+1) zs acc
stringIP 0 (z:zs) acc = stringIP 1 zs (show z:acc)
stringIP n (z:zs) acc = stringIP (n+1) zs $ (c ++ elemP n):acc
  where c = case z of
          1 -> ""
          -1 -> "-"
          _ -> show z

elemP :: Int -> String
elemP 1 = "x"
elemP n = "x^" ++ show n

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P la) (P lb)= P $ plusP la lb

plusP :: Num a => [a] -> [a] -> [a]
plusP [] [] = []
plusP [] lb = lb
plusP la [] = la
plusP (a:la) (b:lb) = a+b : plusP la lb

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P la) (P lb) = P $ foldr plusP [] $ zipWith shift [0..length lb - 1] $ map (timesE la) lb
  where
  timesE :: (Num a) => [a] -> a -> [a]
  timesE l e = map (*e) l

  shift :: (Num a) => Int -> [a] -> [a]
  shift 0 l = l
  shift n l = 0:shift (n-1) l
{-
times (P la) (P lb) = P $ timesP la lb

timesP :: Num a => [a] -> [a] -> [a]
timesP [] _ = []
timesP (a:as) bs = plusP (map (a*) bs) (0:timesP as bs)
-}

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
  (+) = plus
  (*) = times
  negate (P l) = P $ map negate l
  fromInteger = P . (:[]) . fromInteger
  -- No meaningful definitionsundefined exist
  abs    = undefined
  signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P l) v = foldr (\a b -> a + v * b) 0 l

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 = id
    nderiv 1 = deriv
    nderiv n = nderiv (n-1) . deriv

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
  deriv (P []) = P []
  deriv (P (_:l)) = P l + x * deriv (P l)
