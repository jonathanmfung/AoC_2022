import Data.Bifunctor (second)

digitToVal :: Char -> Int
digitToVal c =
  case c of
    '2' -> 2
    '1' -> 1
    '0' -> 0
    '-' -> (-1)
    '=' -> (-2)

snafuToDecimal :: String -> Int
snafuToDecimal sns = sum $ zipWith (*) snafu base
  where
    snafu = reverse $ digitToVal <$> sns
    base = [5 ^ x | x <- [0 ..]]

decimalToFive :: Int -> String
decimalToFive dec =
  reverse $
    concatMap
      (show . snd)
      rems
  where
    -- https://stackoverflow.com/a/41776706
    takeWhileP1 p xs = map snd (takeWhile fst (zip (True : map p xs) xs))
    rems =
      tail $
        takeWhileP1 ((> 0) . fst) $
          iterate
            (\(i, r) -> quotRem i 5)
            (dec, 0)

fiveToSnafu :: String -> String
fiveToSnafu s = reverse $ go (reverse s) False
  where
    go :: String -> Bool -> String
    go (x : xs) n = new : go xs rmdr
      where
        x' = if n then succ x else x
        (new, rmdr) = reduce x'
    go x n = if n then "1" else [] -- last step has carry over
    reduce c = case c of
      '5' -> ('0', True)
      '4' -> ('-', True)
      '3' -> ('=', True)
      _ -> (c, False)

main1 = do
  inp <- readFile "25_input.txt"
  let sumDec = sum $ snafuToDecimal <$> lines inp
      sumSnafu = fiveToSnafu . decimalToFive $ sumDec
  print sumSnafu

-- answer: 2-00=12=21-0=01--000

-----------
test n =
  takeWhileP1 ((> 0) . fst) $
    iterate
      (\(i, r) -> second (+ r) $ divMod i 5)
      (n, 0)
  where
    takeWhileP1 p xs = map snd (takeWhile fst (zip (True : map p xs) xs))

-- decimal 198
-- is 1243 in base 5
-- is 2=0= in base snafu
--
-- carry over from RHS, digit must be >= 3
-- 1243
-- -> 125=
-- -> 130=
-- -> 2=0=

-- decimal 906 is 12111 base 5
-- 12111
-- RHS 1 is < 3, so cannot carry over

-- decimal 353 is 2403 base 5
-- 2403
-- ->  241=
-- ->  3-1=
-- -> 1=-1=

-- decimal 201 is 1301 base 5
-- 1301
-- -> 2=01

-- decimal 1747 is 23442 base 5
-- 23442
-- -> 235-2
-- -> 240-2
-- -> 3-0-2
-- -> 1=-0-2
