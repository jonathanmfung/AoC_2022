


import Data.Foldable (foldl')

data Inst = Addx !Reg | Noop
  deriving (Show)

type Cycle = Int
type Reg = Int

type State = (Cycle, Reg)

readInst :: String -> Inst
readInst xs = if h == "addx"
              then Addx l
              else Noop
  where
    ws = words xs
    h = head ws
    l :: Reg
    l = read $ last ws

exec :: State -> Inst -> State
exec s@(c, r) i = case i of
                    Addx n -> (c + 2, r + n)
                    Noop -> (c + 1, r)
                    -- Addx n -> [(c + 1, r), (c + 2, r + n)]
                    -- Noop -> [(c + 1, r)]

-- need to concat map to get value at every cycle

sigStrength :: State -> Int
sigStrength (r, c) = r * c


initState :: State
initState = (0, 1)

refCycles :: [Cycle]
refCycles = (:) 20 $ (+20) . (*40) <$> [1..5]


lookupFloor :: Cycle -> [State] -> Reg
lookupFloor c m = case lookup (c - 1) m of
                   Just val -> val
                   Nothing -> lookupFloor (c - 1) m

ex :: [State]
ex = scanl exec initState [Noop, Addx 3, Addx (-5)]


main1 = do
  inp <- readFile "10_input.txt"
  let instrs = readInst <$> lines inp
  let sts = scanl exec initState instrs
  let regs = map (`lookupFloor` sts) refCycles
  print $ sum $ zipWith (*) regs refCycles
-- answer: 14220


-- Part 2

exInp = "addx 15\n\
        \addx -11\n\
        \addx 6\n\
        \addx -3\n\
        \addx 5\n\
        \addx -1\n\
        \addx -8\n\
        \addx 13\n\
        \addx 4\n\
        \noop\n\
        \addx -1"

exInst = readInst <$> lines exInp

-- to start, treat positions as 1D list
-- break lines after everything



-- crt :: Int
-- crt = [0..20]

-- TODO Sprite is cut off at edges, so need to wrap to start
spritePixels :: Reg -> [Int]
spritePixels r = [r - 1, r, r +1]

drawPixel :: Int -> Reg -> Char
drawPixel c r = if c `elem` (spritePixels r)
                   then '#' else '.'

drawCRT :: [Int] -> [Reg] -> [Char]
drawCRT = zipWith drawPixel


handCycle = [1, 1, 16, 16, 5,
                         5, 11, 11, 8, 8,
                         13, 13, 12, 12,
                         4, 4, 17, 17, 21, 21, 21]

xCycle = map (`lookupFloor` (scanl exec initState exInst)) [1..21]

exOut = drawCRT [0..20] handCycle

main2 :: IO ()
main2 = do
  inp <- readFile "10_input.txt"
  let insts = readInst <$> lines inp
  let sts = scanl exec initState insts
  let cycs = map (`lookupFloor` sts) [1..241]
  let res = drawCRT [0..240] cycs
  print $ res
