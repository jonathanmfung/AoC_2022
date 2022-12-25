{-# LANGUAGE OverloadedStrings #-}
-- Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

import qualified Data.Text as T
import Data.List

splitInv :: T.Text -> [T.Text]
splitInv = T.splitOn "\n\n"

splitItem :: T.Text -> [T.Text]
splitItem = T.splitOn "\n"

calSum :: [T.Text] -> Int
calSum ts = sum $ map (read . T.unpack) ts

groupToInt :: T.Text -> Int
groupToInt = calSum . (filter (/= T.pack "") . splitItem)

res :: [Int]
res = groupToInt <$> splitInv "1\n2\n\n3\n4\n"


main :: IO ()
main = do
  inp <- readFile "01_input.txt"
  let cals = groupToInt <$> splitInv (T.pack inp)
  print $ maximum cals
-- answer: 65912


-- part 2
main2 :: IO ()
main2 = do
  inp <- readFile "01_input.txt"
  let cals = groupToInt <$> splitInv (T.pack inp)
  print $ sum $ take 3 $ reverse . sort $ cals
-- answer: 195625
