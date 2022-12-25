{-# LANGUAGE TupleSections #-}

import Data.Bifunctor

import Data.Foldable
import qualified Data.HashSet as HS
import Data.List
-- import qualified Data.IntMap as HS


type Coord = (Int, Int) -- (X, Y)

readSensorBeacon :: String -> (Coord, Coord)
readSensorBeacon xs = ((sx, sy), (bx, by))
  where
    ws = filter (/= ':') . filter (/= ',') . drop 2 <$> words xs
    sx = read $ ws !! 2
    sy = read $ ws !! 3
    bx = read $ ws !! 8
    by = read $ ws !! 9

manhattan :: Coord -> Coord -> Int
manhattan (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

type Range = Int

type Sensor = (Coord, Range)

-- generate range of all sensors
-- generate diamond with specified manhattan, then translate to beacon

genRange :: Int -> [Coord]
genRange d = concat [q1, q2, q3, q4, axes, origin]
  where
    q1 = filter (\(a, b) -> a + b <= d) [(x, y) | x <- [1 .. d], y <- [1 .. d]]
    q2 = first ((-1) *) <$> q1
    q3 = bimap ((-1) *) ((-1) *) <$> q1
    q4 = second ((-1) *) <$> q1
    axes = map (0,) [1 .. d] ++ map (,0) [1 .. d]
    origin = [(0,0)]

transRange :: [Coord] -> Coord -> [Coord]
transRange range sns@(dx, dy) = map (\(a, b) -> (a + dx, b + dy)) range

sensorRange :: (Coord, Coord) -> [Coord]
sensorRange (sns, bcn) = transRange (genRange $ manhattan sns bcn) sns


type Tunnel = HS.HashSet Coord

-- then find empty slots
unfilled :: Int -> Tunnel -> Int
unfilled nrow tn = HS.size $ HS.difference fullrow tnrow
  where
    xs = map fst $ HS.toList tn
    minx = minimum xs
    maxx = maximum xs
    tnrow = HS.filter (\(a,b) -> b == nrow) tn
    fullrow :: Tunnel
    fullrow = HS.fromList $ map (,nrow) [minx .. maxx]

crosses :: Int -> (Coord, Coord) -> Bool
crosses n (sns@(_, y1), bcn) = (miny < n && n < maxy) || (maxy < n && n < miny)
  where
    d = manhattan sns bcn
    maxy = y1 + d
    miny = y1 - d
-- need to calculate manhattan, then extend both positive and negative

mInsert m ns = foldl' (flip HS.insert) m ns



atRow :: Int -> (Coord, Coord) -> [Int]
-- calculate range only at line n
atRow n (sns@(x1, y1), bcn) = [(x1 - delt) .. (x1 + delt)]
  where
    d = manhattan sns bcn
    delt = d - abs (y1 - n)

main1 = do
  inp <- readFile "15_input.txt"
  let n = 2000000
  let pairs = filter (crosses n) $ readSensorBeacon <$> lines inp
  let allx = map (fst . fst) pairs ++ map (fst . snd) pairs
  let minx = minimum allx
  let maxx = maximum allx
  let ranges = HS.fromList $ concat $ (atRow n) <$> pairs
  let possible = HS.fromList $ [(minimum $ HS.toList ranges)..(maximum $ HS.toList ranges)]
  -- print $ (minimum $ HS.toList ranges, maximum $ HS.toList ranges)
  -- print (minx, maxx)
  print $ HS.size $ HS.difference possible ranges

main1' = do
  inp <- readFile "15_input.txt"
  let n = 2000000
  let orig = readSensorBeacon <$> lines inp
  -- see if there are any irrelevant pairs
  let pairs = filter (crosses n) orig
  print (length orig, length pairs)
