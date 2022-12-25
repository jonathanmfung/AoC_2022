{-# LANGUAGE LambdaCase #-}

data Hand = Rock | Paper | Scissors
  deriving (Show, Enum, Eq)

winsTo :: Hand -> Hand
winsTo Scissors = Rock
winsTo x = succ x

losesTo :: Hand -> Hand
losesTo Rock = Scissors
losesTo x = pred x

verify = (winsTo . losesTo <$> enums) == (losesTo . winsTo <$> enums)
  where enums = [Rock, Paper, Scissors]

toHand :: String -> Hand
toHand s = case s of
  "A" -> Rock
  "X" -> Rock
  "B" -> Paper
  "Y" -> Paper
  "C" -> Scissors
  "Z" -> Scissors


data Result = Win | Lose | Draw
  deriving Show

-- Result is based on the second column
battle :: (Hand, Hand) -> Result
battle (l, r) = case (l, r) of
  (Rock, Paper) -> Win
  (Paper, Scissors) -> Win
  (Scissors, Rock) -> Win
  (Rock, Rock) -> Draw
  (Paper, Paper) -> Draw
  (Scissors, Scissors) -> Draw
  _ -> not' $ battle (r, l)
  where
    not' :: Result -> Result
    not' Win = Lose
    not' Lose = Win
    not' Draw = Draw

handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

resultScore :: Result -> Int
resultScore Lose = 0
resultScore Draw = 3
resultScore Win = 6

roundScore :: Hand -> Hand -> Int
roundScore l r = handScore r + resultScore (battle (l, r))

main = do
  inp <- readFile "02_input.txt"
  let res = (\[x,y] -> roundScore x y) . map toHand . words <$> lines inp
  print $ sum res
-- answer: 12772


-- part 2
toHand' :: String -> Hand
toHand' s = case s of
  "A" -> Rock
  "B" -> Paper
  "C" -> Scissors

toRes :: String -> Result
toRes s = case s of
  "Y" -> Draw
  "X" -> Lose
  "Z" -> Win

solveSnd :: Result -> Hand -> Hand
solveSnd = \case
  Draw -> id
  Win -> winsTo
  Lose -> losesTo

roundScore' :: Result -> Hand -> Int
roundScore' res l = roundScore l r
  where
    r = solveSnd res l

main' = do
  inp <- readFile "02_input.txt"
  let res = (\[x,y] -> roundScore' (toRes y) (toHand' x)) . words <$> lines inp
  print $ sum res
-- answer: 11618
