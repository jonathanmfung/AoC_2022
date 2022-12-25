{-# LANGUAGE OverloadedStrings #-}
-- Did not solve

import Data.Bifunctor (bimap, first, second)
import Data.Foldable (foldl')
import Data.List (intersect)

       --
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Bifunctor as B
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.List as L


indexEvery :: [a] -> Int -> [a]
indexEvery xs n = map (xs !!) is
  where
    l = length xs - 1
    is = [x | x <- [0 .. l], mod x n == 0]

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = (map head x) : transpose (map tail x)

type Row = String

readRow :: String -> Row
readRow s = indexEvery (tail s) 4

type Col = String

readCols :: [Row] -> [Col]
readCols = transpose . fmap readRow

data Move = Move
  { n :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

readMove :: String -> Move
readMove s = Move (read n) (read f) (read t)
  where
    ls = words s
    n = ls !! 1
    f = ls !! 3
    t = ls !! 5

splitRowMove :: [String] -> ([String], [String])
splitRowMove = bimap init tail . span (/= "")

deleteAt :: Int -> Int -> [a] -> ([a], [a], [a])
deleteAt a b xs =
  if a < b
    then (l1, m1, r1)
    else (l2, m2, snd)
  where
    tail' [] = []
    tail' xs = tail xs
    init' [] = []
    init' xs = init xs

    (l1, snd) = second tail' $ splitAt a xs
    newb = b - length l1
    (m1, r1) = first init' $ splitAt newb snd
    (l2, m2) = first init' $ splitAt a l1

arrange :: Int -> (String, String) -> (String, String)
arrange n (from, to) = (remain, push carry to)
  where
    pop :: Int -> [a] -> ([a], [a])
    pop n xs = splitAt (length xs - n) xs
    push :: [a] -> [a] -> [a]
    push new xs = xs ++ reverse new
    (remain, carry) = pop n from

-- arrange 1 ("MCD", "ZN")

rearrange :: [String] -> [Move] -> [String]
rearrange xs mvs = foldl' go xs mvs
  where
    go :: [String] -> Move -> [String]
    go xs (Move n f t) =
      let fInd = f - 1
          tInd = t - 1
          (fNew, tNew) =
            arrange
              n
              ( xs !! fInd,
                xs !! tInd
              )
          (l, m, r) = deleteAt fInd tInd xs
       in if fInd < tInd
            then [fNew] ++ m ++ l ++ [tNew] ++ r
            else l ++ [tNew] ++ m ++ [fNew] ++ r

testCol = ["ZN", "MCD", "P"]

testMove =
  [ Move 1 2 1,
    Move 3 1 3,
    Move 2 2 1,
    Move 1 1 2
  ]

actCol = ["ZTFRWJG", "GWM", "JNHG", "JRCNW", "WFSBGQVM", "SRTDVWC", "HBNCDZGV", "SJNMGC", "GPNWCJDL"]

actMove =
  [ Move 1 5 2,
    Move 7 7 1,
    Move 1 1 7,
    Move 1 4 1,
    Move 7 9 1,
    Move 1 3 7,
    Move 4 5 4,
    Move 6 4 9,
    Move 2 7 6,
    Move 6 8 2, -- 10
    Move 2 4 5,
    Move 2 3 7,
    Move 11 1 4,
    Move 6 6 1,
    Move 3 5 3,
    Move 5 9 8,
    Move 1 2 3,
    Move 2 7 9,
    Move 7 1 2,
    Move 1 5 3,
    Move 1 5 3,
    Move 5 8 5,
    Move 3 5 4,
    Move 1 1 7,
    Move 1 3 8,
    Move 2 6 3,
    Move 3 3 4,
    Move 1 6 2,
    Move 5 4 2,
    Move 2 5 3,
    Move 2 7 1,
    Move 1 8 1,
    Move 7 1 7,
    Move 4 4 2,
    Move 7 4 1,
    Move 10 1 5,
    Move 10 5 2,
    Move 11 2 3,
    Move 1 1 6,
    Move 1 4 7,
    Move 4 7 1,
    Move 6 2 5,
    Move 2 1 3,
    Move 1 9 5,
    Move 2 9 6,
    Move 1 6 1,
    Move 3 5 4,
    Move 20 3 9,
    Move 3 7 1,
    Move 3 5 2,
    Move 3 4 8,
    Move 3 1 3,
    Move 3 1 2,
    Move 2 6 1,
    Move 10 9 6,
    Move 6 6 7,
    Move 4 6 3,
    Move 11 2 6,
    Move 1 8 9,
    Move 13 2 3,
    Move 1 1 9,
    Move 1 9 4,
    Move 1 8 2,
    Move 1 8 2,
    Move 4 7 8,
    Move 8 6 9,
    Move 3 2 3,
    Move 3 8 4,
    Move 11 9 2,
    Move 7 9 6,
    Move 1 1 5,
    Move 4 4 9,
    Move 21 3 1,
    Move 1 3 9,
    Move 7 6 3,
    Move 6 1 2,
    Move 13 1 5,
    Move 2 1 2,
    Move 3 9 3,
    Move 2 2 3,
    Move 2 6 4,
    Move 3 3 5,
    Move 13 5 2,
    Move 5 3 4,
    Move 2 7 9,
    Move 2 4 2,
    Move 1 3 8,
    Move 1 6 1,
    Move 4 3 7,
    Move 2 5 7,
    Move 1 7 2,
    Move 1 5 9,
    Move 4 7 8,
    Move 1 1 9,
    Move 6 8 1,
    Move 4 4 8,
    Move 25 2 9,
    Move 1 4 3,
    Move 1 3 7,
    Move 4 8 1,
    Move 1 7 4,
    Move 3 1 6,
    Move 5 2 1,
    Move 1 5 1,
    Move 1 4 1,
    Move 24 9 6,
    Move 9 1 6,
    Move 1 5 6,
    Move 1 1 9,
    Move 1 2 8,
    Move 1 8 1,
    Move 3 1 8,
    Move 36 6 3,
    Move 2 7 3,
    Move 1 2 5,
    Move 1 5 2,
    Move 1 6 2,
    Move 10 3 2,
    Move 3 8 2,
    Move 1 1 7,
    Move 2 2 6,
    Move 10 9 1,
    Move 2 6 4,
    Move 13 3 4,
    Move 8 3 7,
    Move 8 1 2,
    Move 5 3 8,
    Move 3 1 9,
    Move 1 7 1,
    Move 7 4 5,
    Move 1 1 2,
    Move 14 2 6,
    Move 2 7 2,
    Move 8 4 8,
    Move 3 7 9,
    Move 2 9 8,
    Move 2 7 1,
    Move 1 7 8,
    Move 1 6 8,
    Move 1 9 3,
    Move 4 2 7,
    Move 6 6 1,
    Move 3 1 9,
    Move 1 1 7,
    Move 6 5 6,
    Move 1 5 2,
    Move 1 6 8,
    Move 5 7 5,
    Move 1 2 9,
    Move 2 3 4,
    Move 9 8 4,
    Move 8 4 8,
    Move 6 6 7,
    Move 5 6 4,
    Move 7 9 7,
    Move 7 8 7,
    Move 5 8 4,
    Move 3 1 6,
    Move 1 2 7,
    Move 1 1 4,
    Move 4 5 2,
    Move 2 6 9,
    Move 1 3 7,
    Move 1 5 1,
    Move 1 8 9,
    Move 1 6 1,
    Move 1 2 7,
    Move 2 8 1,
    Move 2 1 8,
    Move 3 2 4,
    Move 1 6 1,
    Move 17 4 1,
    Move 3 2 7,
    Move 13 7 8,
    Move 1 2 6,
    Move 14 1 4,
    Move 2 8 5,
    Move 1 9 7,
    Move 2 5 4,
    Move 1 9 3,
    Move 5 1 5,
    Move 3 4 1,
    Move 1 3 2,
    Move 7 4 5,
    Move 9 7 8,
    Move 5 4 2,
    Move 1 1 3,
    Move 1 9 2,
    Move 15 8 6,
    Move 1 3 7,
    Move 11 6 5,
    Move 1 4 8,
    Move 3 1 7,
    Move 5 7 5,
    Move 27 5 1,
    Move 8 8 4,
    Move 1 2 6,
    Move 3 6 1,
    Move 9 1 5,
    Move 5 5 7,
    Move 2 2 1,
    Move 2 5 4,
    Move 6 7 6,
    Move 1 5 2,
    Move 1 7 8,
    Move 4 6 8,
    Move 5 6 3,
    Move 1 7 1,
    Move 5 4 3,
    Move 6 8 2,
    Move 1 7 8,
    Move 2 8 9,
    Move 10 3 5,
    Move 9 5 2,
    Move 3 4 8,
    Move 1 5 7,
    Move 2 9 7,
    Move 2 8 3,
    Move 1 3 8,
    Move 19 1 7,
    Move 4 2 7,
    Move 2 4 3,
    Move 3 3 2,
    Move 2 8 3,
    Move 2 5 8,
    Move 1 2 3,
    Move 2 8 3,
    Move 5 2 5,
    Move 9 7 5,
    Move 13 5 9,
    Move 7 2 6,
    Move 2 6 9,
    Move 1 2 1,
    Move 5 6 7,
    Move 1 5 7,
    Move 6 1 2,
    Move 5 3 6,
    Move 6 7 2,
    Move 3 6 4,
    Move 3 7 4,
    Move 12 7 6,
    Move 5 4 1,
    Move 2 7 4,
    Move 3 4 6,
    Move 16 6 3,
    Move 4 1 4,
    Move 1 1 9,
    Move 3 9 2,
    Move 1 4 6,
    Move 9 3 7,
    Move 2 6 3,
    Move 3 3 9,
    Move 15 2 7,
    Move 19 7 4,
    Move 15 9 2,
    Move 16 2 8,
    Move 6 3 5,
    Move 4 7 5,
    Move 15 8 7,
    Move 19 4 2,
    Move 1 8 3,
    Move 16 2 1,
    Move 9 7 6,
    Move 7 2 8,
    Move 2 2 7,
    Move 1 9 5,
    Move 1 3 4,
    Move 6 1 2,
    Move 8 5 1,
    Move 1 5 1,
    Move 18 1 8,
    Move 7 7 5,
    Move 7 5 3,
    Move 4 3 6,
    Move 13 8 5,
    Move 12 8 1,
    Move 5 1 6,
    Move 15 5 4,
    Move 1 1 6,
    Move 12 6 3,
    Move 8 3 4,
    Move 2 7 3,
    Move 9 3 1,
    Move 5 2 9,
    Move 16 4 3,
    Move 10 1 3,
    Move 2 1 5,
    Move 1 3 1,
    Move 5 6 1,
    Move 4 9 3,
    Move 1 2 8,
    Move 1 8 1,
    Move 1 9 8,
    Move 2 5 9,
    Move 9 4 1,
    Move 3 1 3,
    Move 2 6 8,
    Move 3 8 5,
    Move 2 1 5,
    Move 2 9 8,
    Move 1 8 6,
    Move 2 5 3,
    Move 19 3 1,
    Move 2 4 2,
    Move 1 5 6,
    Move 2 2 3,
    Move 1 8 6,
    Move 8 3 9,
    Move 6 3 7,
    Move 2 6 2,
    Move 1 6 1,
    Move 1 1 8,
    Move 1 8 9,
    Move 1 7 3,
    Move 19 1 5,
    Move 21 5 2,
    Move 13 2 6,
    Move 13 1 8,
    Move 7 9 7,
    Move 2 9 2,
    Move 10 8 3,
    Move 1 1 6,
    Move 10 2 4,
    Move 11 3 5,
    Move 8 5 6,
    Move 1 3 7,
    Move 2 8 6,
    Move 2 2 8,
    Move 3 7 6,
    Move 2 8 6,
    Move 1 1 2,
    Move 24 6 5,
    Move 2 3 8,
    Move 1 8 6,
    Move 7 7 9,
    Move 4 6 9,
    Move 1 8 9,
    Move 21 5 9,
    Move 2 7 2,
    Move 1 8 5,
    Move 1 7 3,
    Move 12 9 6,
    Move 6 6 3,
    Move 12 9 4,
    Move 4 5 6,
    Move 13 4 2,
    Move 8 4 8,
    Move 10 6 8,
    Move 11 8 9,
    Move 4 8 4,
    Move 2 4 3,
    Move 8 3 8,
    Move 2 6 8,
    Move 1 3 8,
    Move 6 2 4,
    Move 1 4 8,
    Move 1 9 7,
    Move 13 8 4,
    Move 1 7 1,
    Move 1 1 4,
    Move 8 4 7,
    Move 3 5 7,
    Move 19 9 7,
    Move 3 2 7,
    Move 1 8 2,
    Move 13 7 6,
    Move 1 2 4,
    Move 4 6 2,
    Move 1 8 3,
    Move 7 6 8,
    Move 1 6 2,
    Move 1 2 7,
    Move 9 2 3,
    Move 1 6 2,
    Move 21 7 5,
    Move 9 5 3,
    Move 19 3 9,
    Move 5 8 5,
    Move 2 2 1,
    Move 2 1 8,
    Move 6 4 5,
    Move 3 8 7,
    Move 15 9 2,
    Move 2 2 5,
    Move 3 9 6,
    Move 5 4 5,
    Move 11 2 6,
    Move 1 8 6,
    Move 1 9 5,
    Move 1 7 3,
    Move 6 5 6,
    Move 1 4 6,
    Move 1 3 4,
    Move 13 5 2,
    Move 16 6 9,
    Move 4 4 5,
    Move 2 6 2,
    Move 2 6 4,
    Move 2 4 5,
    Move 2 7 8,
    Move 2 6 3,
    Move 2 5 8,
    Move 14 5 7,
    Move 4 8 1,
    Move 4 1 6,
    Move 1 3 9,
    Move 1 6 1,
    Move 2 7 3,
    Move 2 3 7,
    Move 2 5 2,
    Move 9 9 2,
    Move 13 7 3,
    Move 12 3 9,
    Move 2 6 8,
    Move 14 2 9,
    Move 2 8 9,
    Move 10 2 1,
    Move 1 7 4,
    Move 2 3 8,
    Move 4 2 1,
    Move 1 8 3,
    Move 1 2 6,
    Move 1 8 3,
    Move 4 9 4,
    Move 1 3 5,
    Move 1 5 1,
    Move 1 3 9,
    Move 12 1 8,
    Move 10 8 5,
    Move 7 5 6,
    Move 1 1 9,
    Move 3 5 1,
    Move 1 1 3,
    Move 16 9 7,
    Move 4 4 3,
    Move 1 4 9,
    Move 15 7 8,
    Move 15 9 1,
    Move 8 1 6,
    Move 1 9 3,
    Move 17 6 2,
    Move 1 9 1,
    Move 15 2 7,
    Move 14 8 9,
    Move 12 7 9,
    Move 12 9 3,
    Move 3 7 9,
    Move 1 7 4,
    Move 7 9 6,
    Move 1 4 6,
    Move 11 9 6,
    Move 2 1 2,
    Move 18 6 4,
    Move 4 2 7,
    Move 2 7 3,
    Move 2 7 8,
    Move 4 1 5,
    Move 1 9 2,
    Move 2 5 4,
    Move 5 1 3,
    Move 2 3 7,
    Move 2 3 9,
    Move 1 6 7,
    Move 1 2 9,
    Move 2 8 1,
    Move 3 1 3,
    Move 2 5 8,
    Move 2 3 5,
    Move 1 5 2,
    Move 1 1 3,
    Move 1 9 2,
    Move 1 9 1,
    Move 3 7 6,
    Move 1 1 9,
    Move 2 8 9,
    Move 1 2 3,
    Move 2 8 2,
    Move 2 6 5,
    Move 1 8 5,
    Move 3 2 5,
    Move 3 4 8,
    Move 1 8 2,
    Move 3 9 7,
    Move 3 7 1,
    Move 1 9 6,
    Move 3 1 2,
    Move 2 8 7,
    Move 2 7 9,
    Move 2 6 5,
    Move 3 5 3,
    Move 1 2 5,
    Move 3 2 7,
    Move 2 5 6,
    Move 15 4 9,
    Move 1 3 1,
    Move 25 3 4,
    Move 3 7 3,
    Move 5 9 5,
    Move 10 9 5,
    Move 9 5 1,
    Move 5 5 2,
    Move 1 6 7,
    Move 5 5 8
  ]

main1 :: IO ()
main1 = do
  inp <- readFile "05_input.txt"
  let (rows, moves) = splitRowMove $ lines inp
  let cols = filter (/= ' ') <$> readCols rows
  let mvs = map readMove moves
  print $ reverse <$> cols



    --

-- https://www.reddit.com/r/haskell/comments/zcxhuc/advent_of_code_2022_day_5/iyz8r5m/

{-
Day 5: Supply Stacks
The expedition can depart as soon as the final supplies have been
unloaded from the ships. Supplies are stored in stack of marked crates,
but because the needed supplies are buried under many other crates, the
crates need to be rearranged.
The giant cargo crane can rearrange the crates in a series of carefully-planned
steps. After the crates are rearranged, the desired crates will be at the top
of each stack.
The Elves don't want to interrupt the process, but they forgot to ask
which crate will end up where, and they want to be ready to unload them
as soon as possible so they can embark.
-}

type ProcedureManual = T.Text
type CratePriorityMessage = T.Text

cleanSplit :: Eq a => a -> [a] -> ([a], [a])
cleanSplit char x = B.second tail $ span (/= char) x

parseInitialStackCondition :: [T.Text] -> [T.Text]
parseInitialStackCondition xs = filter (/= "") $ fmap (T.pack . filter C.isAlpha) $ L.transpose $ T.unpack <$> xs

parseInstruction :: T.Text -> [Int]
parseInstruction x = read . T.unpack <$> T.words (T.filter (\x -> C.isDigit x || C.isSpace x) x)

executeInstruction :: Bool -> [T.Text] -> [Int] -> [T.Text]
executeInstruction shouldReverse xs i = (\(x, y) ->
    if y == from
        then T.drop (T.length cratesTaken) x
    else if y == to
        then cratesTaken <> x
    else x) <$> zip xs [0..]
    where
        cratesTaken = (if shouldReverse then T.reverse else id) $ T.take crates (xs !! from)
        crates = head i
        from = 1 `subtract` (i !! 1)
        to = 1 `subtract` (i !! 2)

-- determine which crate will end up on top of each stack
resultOfRearrangement :: ProcedureManual -> CratePriorityMessage
resultOfRearrangement x = T.pack $ fmap T.head $ (\(x, y) -> L.foldl' (executeInstruction True) (parseInitialStackCondition x) (parseInstruction <$> y)) $ cleanSplit "" $ T.lines x

-- determine which crate will end up on top if the crates remain in the same order while being moved
resultOfRearrangement' :: ProcedureManual -> CratePriorityMessage
resultOfRearrangement' x = T.pack $ fmap T.head $ (\(x, y) -> L.foldl' (executeInstruction False) (parseInitialStackCondition x) (parseInstruction <$> y)) $ cleanSplit "" $ T.lines x


main1' = do
  inp <- TIO.readFile "05_input.txt"
  TIO.putStrLn $ resultOfRearrangement inp

main2' = do
  inp <- TIO.readFile "05_input.txt"
  TIO.putStrLn $ resultOfRearrangement' inp
