{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Worry = Int

data Monkey = Monkey
  { items :: [Worry],
    op :: Worry -> Worry,
    test :: Worry -> Int
  }

instance Show Monkey where
  show m = show (items m) ++ "\n" ++ show (op m 10) ++ "\n" ++ show (test m 10) ++ "\n"

readItems :: String -> [Worry]
readItems xs = map (read . filter (/= ',')) ws
  where
    ws = drop 2 $ words xs

readOp :: String -> (Worry -> Worry)
readOp xs = case op of
  "*" -> case snd of
    "old" -> \w -> w * w
    n@otherwise -> \w -> w * (read n)
  "+" -> case snd of
    "old" -> \w -> w + w
    n@otherwise -> \w -> w + (read n)
  where
    ws = drop 4 $ words xs
    op = ws !! 0
    snd = ws !! 1

readTest :: String -> (Worry -> Int)
readTest xs = \w ->
  if w `mod` cond == 0
    then t
    else f
  where
    ls :: [Int]
    ls = map (read . last . words) $ lines xs
    cond = ls !! 0
    t = ls !! 1
    f = ls !! 2

readMonkey :: String -> Monkey
readMonkey xs =
  Monkey
    { items = readItems i,
      op = readOp o,
      test = readTest $ concatMap (\x -> x ++ "\n") t
    }
  where
    ls = lines xs
    i = ls !! 1
    o = ls !! 2
    t = drop 3 ls

exMonk =
  "Monkey 0:\n\
  \Starting items: 92, 73, 86, 83, 65, 51, 55, 93\n\
  \Operation: new = old * 5\n\
  \Test: divisible by 11\n\
  \If true: throw to monkey 3\n\
  \If false: throw to monkey 4"

updateMonkey :: Maybe Int -> Maybe Worry -> [Monkey] -> [Monkey]
updateMonkey Nothing Nothing ms = ms
updateMonkey (Just ind) (Just w) ms = l ++ new : r
  where
    (l,r') = splitAt ind ms
    old = head r'
    r = tail r'
    new = old {items = items old ++ [w]}

updateMonkey' :: Int -> Maybe [Worry] -> [Monkey] -> [Monkey]
updateMonkey' _ Nothing ms = ms
updateMonkey' ind (Just ws) ms = l ++ new : r
  where
    (l,r') = splitAt ind ms
    old = head r'
    r = tail r'
    new = old { items = ws}

head' [] = Nothing
head' xs = Just $ head xs

tail' [] = Nothing
tail' xs = Just $ tail xs

execMonkey :: [Monkey] -> Int -> [Monkey]
execMonkey ms ind = -- updateMonkey (Just ind) oldItems $
  -- this does not update monkey item is taken from
  updateMonkey' ind oldItems $
  updateMonkey newInd newItem ms
  where
    currMonk = ms !! ind
    currItem = head' $ items currMonk
    oldItems = tail' $ items currMonk
    newItem = ((`div` 3) . op currMonk) <$> currItem
    newInd = test currMonk <$> newItem

execRound :: [Monkey] -> [Monkey]
execRound ms = foldl execMonkey ms [0..length ms - 1]

                     -- (Index, Inspection Count)
                     -- or add a field to Monkey
execTurn :: ([Monkey], [(Int, Int)]) ->([Monkey], [(Int, Int)])
execTurn = undefined

main1 = do
  inp <- TIO.readFile "11_input.txt"
  let monks = readMonkey . T.unpack <$> T.splitOn "\n\n" inp
  let res = (iterate execRound monks) !! 20
  print $ res
