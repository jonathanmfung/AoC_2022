import qualified Data.Text as T
import Data.Foldable (foldl')

type Voxel = (Int, Int, Int)

readVoxel :: String -> Voxel
readVoxel xs = (a, b, c)
  where
    ws = T.splitOn (T.pack ",") (T.pack xs)
    is :: [Int]
    is = read . T.unpack <$> ws
    a = is !! 0
    b = is !! 1
    c = is !! 2

adjacent :: Voxel -> Voxel -> Bool
adjacent a@(x1, y1, z1) b@(x2, y2, z2) =
  (==) 1 . sum $ abs <$> [dx, dy, dz]
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1

-- add voxels one by one to a Storage
-- each time adding, count number of adjacents to Storage
-- surface area is then number of cubes - number of adjacents

data State = State
             Int -- number of adjacents
             [Voxel] -- current voxels
             deriving Show

update :: State -> Voxel -> State
update (State i vs) v = State (i + new) (v : vs)
  where
    new = sum $ map (fromEnum . adjacent v) vs

swallow :: [Voxel] -> State
swallow = foldl' update (State 0 [])

surfaceArea :: State -> Int
surfaceArea (State i vs) = 6 * length vs - 2 * i
                           -- an adjacent takes two faces off the pair, so (2*)

exInp =
  "2,2,2\n\
  \1,2,2\n\
  \3,2,2\n\
  \2,1,2\n\
  \2,3,2\n\
  \2,2,1\n\
  \2,2,3\n\
  \2,2,4\n\
  \2,2,6\n\
  \1,2,5\n\
  \3,2,5\n\
  \2,1,5\n\
  \2,3,5"

exOut = surfaceArea . swallow $ readVoxel <$> lines exInp

main1 = do
  inp <- readFile "18_input.txt"
  let res = surfaceArea . swallow $ readVoxel <$> lines inp
  print res
  -- answer = 3610
