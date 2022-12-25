import Control.Monad (foldM)
import Data.List (intersect)

input = "vJrwpWtwJgWrhcsFMMfFFhFp"

splitRucksack :: [a] -> ([a], [a])
splitRucksack s = splitAt n s
  where
    n = length s `div` 2

getConflict :: (String, String) -> Char
getConflict (s1, s2) = head $ intersect s1 s2

valMap :: [(Char, Int)]
valMap = zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]

main :: IO ()
main = do
  inp <- readFile "03_input.txt"
  let res = (`lookup` valMap) . getConflict . splitRucksack <$> lines inp
  print $ sum <$> sequence res
-- answer: 8153

-- Part 2:
splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = hd : splitN n tl
  where
    (hd, tl) = splitAt n xs

findBadge :: [[Char]] -> [[Char]]
findBadge lines = map (foldl intersect ['A' .. 'z']) $ splitN 3 lines

main2 :: IO ()
main2 = do
  inp <- readFile "03_input.txt"
  let res = map ((`lookup` valMap) . head) $ findBadge $ lines inp
  print $ sum <$> sequence res
-- answer: 2342
