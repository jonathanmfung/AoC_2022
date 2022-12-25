{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.List (union, intersect )


readPair :: String -> [[Int]]
readPair s = fmap (toRange . map (read . T.unpack) . T.splitOn "-") ranges
  where
    ranges = T.splitOn "," $ T.pack s
    toRange :: [Int] -> [Int]
    toRange is = [(head is)..(last is)]

contains :: [[Int]] -> Int
contains is = if unLen <= mLen then 1 else 0
  where
    fst = head is
    lst = last is
    mLen = max (length fst) (length lst)
    unLen = length $ union fst lst

main1 :: IO ()
main1 = do
  inp <- readFile "04_input.txt"
  let res = contains . readPair <$> lines inp
  print $ sum res
-- answer: 444

-- Part 2

overlap :: [[Int]] -> Int
overlap is = if null int then 0 else 1
  where
    fst = head is
    lst = last is
    int = fst `intersect` lst

main2 :: IO ()
main2 = do
  inp <- readFile "04_input.txt"
  let res = overlap . readPair <$> lines inp
  print $ sum res
-- answer: 801
