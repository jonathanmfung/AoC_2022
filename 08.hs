import Data.Bifunctor

ex1 =
  "30373\n\
  \25512\n\
  \65332\n\
  \33549\n\
  \35390"

ex2 =
  "00000\n\
  \01110\n\
  \01210\n\
  \01110\n\
  \00000"

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

eOr = zipWith (||)

eeOr = (zipWith . zipWith) (||)

leftMax :: [Char] -> [Bool]
leftMax xs = go (head xs) xs
  where
    go :: Char -> [Char] -> [Bool]
    go _ [] = []
    go m (x : xs) =
      if x <= m
        then False : go m xs
        else True : go x xs

rowVisible :: [Char] -> [Bool]
rowVisible xs = fixEdges $ overlap xs
  where
    overlap xs = eOr (leftMax xs) (reverse $ leftMax $ reverse xs)
    fixEdges :: [Bool] -> [Bool]
    fixEdges (x : xs) = True : init xs ++ [True]

-- m1 = map rowVisible $ lines example
-- m2 = transpose $ map rowVisible $ transpose $ lines example
-- m3 = eeOr m1 m2

visCount :: String -> Int
visCount inp = sum $ map (length . filter (True ==)) bls
  where
    ls = lines inp
    rowwise = map rowVisible ls
    colwise = transpose $ map rowVisible $ transpose ls
    bls = eeOr rowwise colwise

main1 = do
  inp <- readFile "08_input.txt"
  let res = visCount inp
  print res

-- answer: 1733

-- Part 2

type ViewDistance = (Int, Int, Int, Int) -- Up, Right, Down, Left

rowView :: [Char] -> [(Int, Int)]
rowView xs = zipWith assignNumVis xs ts
  where
    splitAt' :: [Char] -> Int -> ([Char], [Char])
    splitAt' xs n = first init $ splitAt n xs

    ts :: [([Char], [Char])]
    ts = map (splitAt' xs) [1 .. length xs]

    numVis :: Char -> [Char] -> [Int]
    numVis _ [] = []
    numVis cur (x : xs) =
      if cur <= x
        then [1]
        else 1 : numVis cur xs

    assignNumVis :: Char -> ([Char], [Char]) -> (Int, Int)
    assignNumVis elem = bimap (sum . numVis elem . reverse) (sum . numVis elem)

viewCount :: String -> [[Int]]
viewCount inp = (zipWith . zipWith) (\x -> scenicScore . joinDirs x) rowwise colwise
  where
    ls = lines inp
    rowwise = map rowView ls
    colwise = transpose $ map rowView $ transpose ls

    joinDirs :: (Int, Int) -> (Int, Int) -> ViewDistance
    joinDirs (l, r) (u, d) = (u, r, d, l)
    foo = (zipWith . zipWith) joinDirs

scenicScore :: ViewDistance -> Int
scenicScore (u, r, d, l) = product [u, r, d, l]

main2 = do
  inp <- readFile "08_input.txt"
  let res = viewCount inp
  print $ maximum . concat $ res

-- answer: 284648
