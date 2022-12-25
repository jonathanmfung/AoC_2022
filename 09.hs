import Data.Foldable (foldl')
import qualified Data.Map as M

data Coord = Coord
  !Int -- Right is +X
  !Int -- Up    is +Y
  deriving (Show, Eq, Ord)

data Motion
  = Up !Int
  | Rt !Int
  | Dn !Int
  | Lt !Int
  deriving (Show)

readMotion :: String -> Motion
readMotion xs = case d of
  "U" -> Up n
  "R" -> Rt n
  "D" -> Dn n
  "L" -> Lt n
  where
    ws = words xs
    d = head ws
    n = read $ last ws

move :: Coord -> Motion -> Coord
move (Coord x y) m = case m of
  Up n -> Coord x (y + n)
  Rt n -> Coord (x + n) y
  Dn n -> Coord x (y - n)
  Lt n -> Coord (x - n) y

updateTail :: Coord -> Coord -> Coord -- return newTail
updateTail hd@(Coord xh yh) tl@(Coord xt yt) =
  if needsUpdate
    then foldl' move tl [xmv, ymv]
    else tl
  where
    xd = xh - xt
    yd = yh - yt
    needsUpdate = max (abs xd) (abs yd) == 2
    -- xmv + ymv is one of 8 unit vectors
    xmv = Rt $ signum xd
    ymv = Up $ signum yd

type PosMap = M.Map Coord Int

updatePosMap :: Coord -> PosMap -> PosMap
updatePosMap c = M.insertWith (+) c 1

initHead = Coord 0 0

initTail = Coord 0 0

initPM = updatePosMap initTail M.empty

initState = State initHead initTail initPM

data State
  = State
      !Coord -- Head
      !Coord -- Tail
      !PosMap -- PosMap
  deriving (Show)

exec :: State -> Motion -> State
exec (State h t pm) mv = State hNew tNew pmNew
  where
    hNew = move h mv
    tNew = updateTail hNew t
    pmNew = updatePosMap tNew pm

execFold :: State -> [Motion] -> State
execFold s mvs = foldl' exec s mvs'
  where
    mvs' = concatMap decomp mvs
    decomp :: Motion -> [Motion]
    decomp (Up n) = replicate n $ Up 1
    decomp (Rt n) = replicate n $ Rt 1
    decomp (Dn n) = replicate n $ Dn 1
    decomp (Lt n) = replicate n $ Lt 1

visits :: State -> Int
visits (State h t pm) = M.size pm

main1 = do
  inp <- readFile "09_input.txt"
  let mvs = readMotion <$> lines inp
  print $ visits $ execFold initState mvs

-- answer: 6236

ex1 :: [Char]
ex1 =
  "R 4\n\
  \U 4\n\
  \L 3\n\
  \D 1\n\
  \R 4\n\
  \D 1\n\
  \L 5\n\
  \R 2"

ex2 = foldl' move (Coord 0 0) (readMotion <$> lines ex1)


-- Part 2

data BigState
  = BigState
      !Coord -- Head
      !Coord -- Tail1 a
      !Coord -- Tail2 b
      !Coord -- Tail3 c
      !Coord -- Tail4 d
      !Coord -- Tail5 e
      !Coord -- Tail6 f
      !Coord -- Tail7 g
      !Coord -- Tail8 i
      !Coord -- Tail9 j
      !PosMap -- PosMap
  deriving (Show)

execBig :: BigState -> Motion -> BigState
execBig (BigState hd a b c d e f g h i pm) mv =
  BigState hdNew aNew bNew cNew dNew eNew fNew gNew hNew iNew pmNew
  where
    hdNew = move hd mv
    aNew = updateTail hdNew a
    bNew = updateTail aNew b
    cNew = updateTail bNew c
    dNew = updateTail cNew d
    eNew = updateTail dNew e
    fNew = updateTail eNew f
    gNew = updateTail fNew g
    hNew = updateTail gNew h
    iNew = updateTail hNew i
    pmNew = updatePosMap iNew pm

initBigState = BigState initHead
               initTail initTail initTail
               initTail initTail initTail
               initTail initTail initTail
               initPM

execBigFold :: BigState -> [Motion] -> BigState
execBigFold s mvs = foldl' execBig s mvs'
  where
    mvs' = concatMap decomp mvs
    decomp :: Motion -> [Motion]
    decomp (Up n) = replicate n $ Up 1
    decomp (Rt n) = replicate n $ Rt 1
    decomp (Dn n) = replicate n $ Dn 1
    decomp (Lt n) = replicate n $ Lt 1

ex3 :: [Char]
ex3 =
  "R 5\n\
  \U 8\n\
  \L 8\n\
  \D 3\n\
  \R 17\n\
  \D 10\n\
  \L 25\n\
  \U 20"

visitsBig :: BigState -> Int
visitsBig (BigState hd a b c d e f g h i pm) = M.size pm

main2 = do
  inp <- readFile "09_input.txt"
  let mvs = readMotion <$> lines inp
  print $ visitsBig $ execBigFold initBigState mvs
-- answer: 2449
