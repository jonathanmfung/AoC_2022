-- Proud of part 1, for using iterate and some laziness

{-# LANGUAGE TupleSections #-}

import Data.Bifunctor (bimap)
import Data.Foldable
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Maybe

type Coord = (Int, Int)

readRockPath :: String -> [Coord]
readRockPath xs = map (bimap (read . init) read . splitAt 4) ws
  where
    ws = filter (/= "->") $ words xs

data Element = Sand | Rock
  deriving (Show, Eq)

-- https://stackoverflow.com/a/30063912
-- create range even if b < a
range :: (Num a, Enum a) => a -> a -> [a]
range a b = [a, a + signum (b - a) .. b]

expandPathPair :: Coord -> Coord -> [Coord]
expandPathPair (a, b) (c, d)
  | dx == 0 = map (a,) (range b d)
  | otherwise = map (,b) (range a c)
  where
    dx = c - a
    dy = d - b

adjPairs :: [a] -> [(a, a)]
adjPairs = zip <*> tail

expandPath :: [Coord] -> [Coord]
expandPath rs = nub $ concatMap (uncurry expandPathPair) $ adjPairs rs

type Cave = H.HashMap Coord Element -- Nonexistent (Coord, Element) means it is Air

-- https://stackoverflow.com/a/56370519
mInsert m ns c = foldl' (\mp k -> H.insert k c mp) m ns

buildRockPath :: Cave -> [Coord] -> Cave
buildRockPath cv rs = mInsert cv rs Rock

foo =
  buildRockPath H.empty $
    concatMap (expandPath . readRockPath) $
      lines
        "498,4 -> 498,6 -> 496,6\n\
        \503,4 -> 502,4 -> 502,9 -> 494,9"

sandSource :: Coord
sandSource = (500, 0)

dropSand :: (Cave, Maybe Coord) -> (Cave, Maybe Coord)
dropSand (cv, c) = drop (cv, c)
  where
    maxRockY = maximum $ snd <$> H.keys cv
    deleteInsert ok nk v m = H.insert nk v $ H.delete ok m

    canMove :: Coord -> Cave -> Bool
    canMove new cv = isNothing $ H.lookup new cv

    drop :: (Cave, Maybe Coord) -> (Cave, Maybe Coord)
    drop (cv, Just curr@(sx, sy))
      | sy == maxRockY = (cv, Nothing) -- at void
      | canMove dn cv = (deleteInsert curr dn Sand cv, Just dn) -- straight down
      | canMove dl cv = (deleteInsert curr dl Sand cv, Just dl) -- down left
      | canMove dr cv = (deleteInsert curr dr Sand cv, Just dr) -- down right
      | curr == sandSource = (cv, Nothing) -- Source is plugged
      | otherwise = (cv, Just sandSource) -- at rest
      where
        dn = (sx, sy + 1)
        dl = (sx - 1, sy + 1)
        dr = (sx + 1, sy + 1)

steady :: (Cave, Maybe Coord) -> Bool
steady (_, Nothing) = False
steady (_, Just _) = True

restingSand :: Cave -> Int
restingSand cv = numSand (fst steadyState) - 1
  where
    steadyState = last $ takeWhile steady $ tail $ iterate dropSand (cv, Just sandSource)

    numSand :: Cave -> Int
    numSand cv = H.size $ H.filter (== Sand) cv

main1 = do
  inp <- readFile "14_input.txt"
  let rocks = concatMap (expandPath . readRockPath) $ lines inp
  let cave = buildRockPath H.empty rocks
  print $ restingSand cave

-- answer: 808
-- Part 2:


buildCaveFloor :: Cave -> Cave
buildCaveFloor cv = mInsert cv (caveFloor cv) Rock
  where
    caveFloor :: Cave -> [Coord]
    caveFloor cv = expandPath [(500 - (y + 100), y), (500 + (y + 100), y)]
      where
        y = 2 + maximum (snd <$> H.keys cv)

blockingSand :: Cave -> Int
blockingSand cv = numSand (fst steadyState) + 1
  where
    steadyState = last $ takeWhile steady $ tail $ iterate dropSand (cv, Just sandSource)

    numSand :: Cave -> Int
    numSand cv = H.size $ H.filter (== Sand) cv


c :: Cave
c = H.fromList [((499,1), Rock), ((500,1), Rock), ((501,1), Rock)]
a = dropSand (c, Just (500,0))

main2 = do
  inp <- readFile "14_input.txt"
  let rocks = concatMap (expandPath . readRockPath) $ lines inp
  let cave = buildCaveFloor $ buildRockPath H.empty rocks
  print $ blockingSand cave
