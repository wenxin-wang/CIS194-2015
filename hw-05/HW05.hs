{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module HW05 where

import Data.List
import Data.Function (on)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import Control.Applicative (liftA2)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret pa = fmap (BS.pack . filter (0 /=)) . (liftA2 (zipWith xor) `on` (fmap BS.unpack . BS.readFile)) pa

getSecret1 :: FilePath -> FilePath -> IO ByteString
getSecret1 o s = do
  so <- BS.unpack <$> BS.readFile o
  ss <- BS.unpack <$> BS.readFile s
  return $ BS.pack $ filter (0 /=) (zipWith xor so ss)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key f = do
  enc <- BS.readFile $ f++".enc"
  BS.writeFile f $ BS.pack $ on (zipWith xor) BS.unpack (BS.cycle key) enc

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile = fmap decode . BS.readFile

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vicF trF = do
  mvictims <- parseFile vicF :: IO (Maybe [TId])
  mtrs <- parseFile trF
  return $ liftA2 (\victims -> filter (\Transaction {tid = x} -> elem x victims)) mvictims mtrs

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr (
  \Transaction {from = fromP, to = toP, amount = a} ->
  Map.insertWith (+) fromP (-a) . Map.insertWith (+) toP a)
  Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . maximumBy (compare `on` snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flows tids = on (genUndoTs tids) (sortBy (flip compare `on` abs . snd)) payees payers
  where
    (payees, payers) = partition ((>0) . snd) $ filter ((/=0) . snd) $ Map.toList flows

genUndoTs :: [TId] -> [(String, Integer)] -> [(String, Integer)] -> [Transaction]
genUndoTs _ [] [] = []
genUndoTs [] _ _ = undefined
genUndoTs _ [] _ = undefined
genUndoTs _ _ [] = undefined
genUndoTs (t:tids) ((payee, pe):payees) ((payer, pr):payers)
  | s > 0 = Transaction payee payer (-pr) t : genUndoTs tids ((payee, s):payees) payers
  | s < 0 = Transaction payee payer pe t : genUndoTs tids payees ((payer, s):payers)
  | otherwise = Transaction payee payer pe t : genUndoTs tids payees payers
  where
    s = pe + pr

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f = BS.writeFile f . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
