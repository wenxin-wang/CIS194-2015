{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= (return . f)

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 vec = liftM2 (\v1 v2 -> vec // [(i1,v2), (i2,v1)]) (vec !? i1) (vec !? i2)

swapV' :: Vector a -> Int -> Int -> Vector a
swapV' vec i1 i2 = vec // [(i1, vec ! i2), (i2, vec ! i1)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do
  y <- f x
  ys <- mapM f xs
  return $ y:ys

getElts :: [Int] -> Vector a -> Maybe [a]
getElts idxs vec = mapM (vec !?) idxs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec
  | null vec = return Nothing
  | otherwise = liftM (vec !?) $ getRandomR (0, length vec - 1)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = liftM V.fromList $ replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n = liftM V.fromList . replicateM n . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec
  | null vec = return vec
  | otherwise = foldM (\v n -> liftM (swapV' v n) $ getRandomR (0, n)) vec $ reverse [1..length vec - 1]

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec pidx = (lts, p, ges)
  where
    p = vec ! pidx
    (ls, rs) = V.splitAt pidx vec
    (lts, ges) = V.partition (< p) (ls <> V.tail rs)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec
  | null vec = vec
  | otherwise = qsort [ y | y <- xs, y < x ] <> cons x (qsort [ y | y <- xs, y >= x ])
    where
      x = V.head vec
      xs = V.tail vec

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
{-
qsortR vec = do
  pidx <- getRandomR (0, length vec - 1)
  let (lts, p, ges) = partitionAt vec pidx
  slts <- qsortR lts
  sges <- qsortR ges
  return $ slts <> cons p sges
-}
qsortR vec
  | null vec = return vec
  | otherwise = getRandomR (0, length vec - 1) >>=
  \pidx -> let (lts, p, ges) = partitionAt vec pidx
           in liftM2 (<>) (qsortR lts) (liftM (cons p) (qsortR ges))

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select idx vec
  | idx >= length vec = return Nothing
  | otherwise = getRandomR (0, length vec - 1) >>=
  \pidx ->
    let (lts, p, ges) = partitionAt vec pidx
        ll = length lts
    in case compare idx ll of
      LT -> select idx lts
      EQ -> return $ Just p
      GT -> select (idx - ll - 1) ges

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s |
             l <- V.fromList [Two ..],
             s <- V.fromList [Spade, Heart, Club, Diamond]
           ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
  | null deck = Nothing
  | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck = go n ([], deck)
  where
    go :: Int -> ([Card], Deck) -> Maybe ([Card], Deck)
    go 0 ret = return ret
    go m (crds, d) = nextCard d >>= \(crd, r) -> go (m-1) (crd:crds, r)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
-- main = evalRandIO newDeck >>= repl . State 100
main = evalRandIO newDeck >>= repl . State 100
