import Data.List
import Data.Maybe (fromJust)
import Numeric (showFFloat)

data ITree
  = IBranch Char String String
  | IVal Double
  deriving (Show)

type InterTree = (String, ITree)

data Tree
  = Branch String Char Tree Tree
  | Nil String Double
  deriving (Eq)

instance Show Tree where
  show = showAtDepth 0
    where
      showAtDepth l (Branch n op a b) =
        addSpace l ++ show n ++  [' ', op] ++ "\n" ++
        showAtDepth (l + 1) a ++
        showAtDepth (l + 1) b
      showAtDepth l (Nil n v) = addSpace l ++ show n ++ " " ++ show v ++ "\n"

      addSpace = flip replicate '-'

readITree :: String -> InterTree
readITree xs =
  if length ws > 2
    then (name, IBranch op a b)
    else (name, IVal $ read a)
  where
    ws = words xs
    name = filter (/= ':') $ ws !! 0
    a = ws !! 1
    op = head $ ws !! 2
    b = ws !! 3

attach :: InterTree -> [InterTree] -> Tree
attach (n, IVal v) ts = Nil n v
attach (n, IBranch op a b) ts = Branch n op a' b'
  where
    l = lookup n ts
    a' = attach (a, fromJust $ lookup a ts) ts
    b' = attach (b, fromJust $ lookup b ts) ts

findRoot :: [InterTree] -> InterTree
findRoot ts = head $ filter isRoot ts
  where
    isRoot ("root", IBranch {}) = True
    isRoot _ = False

buildTree :: [InterTree] -> Tree
buildTree ts = attach rt ts
  where
    rt = findRoot ts

readOp :: Char -> Double -> Double -> Double
readOp c = case c of
  '+' -> (+)
  '-' -> (-)
  '*' -> (*)
  '/' -> (/)

execute :: Tree -> Double
execute (Nil _ v) = v
execute (Branch _ op a b) = (readOp op) (execute a) (execute b)

exInp =
  "dbpl: 5\n\
  \cczh: sllz + lgvd\n\
  \root: pppw + sjmn\n\
  \zczc: 2\n\
  \ptdq: humn - dvpt\n\
  \dvpt: 3\n\
  \lfqf: 4\n\
  \humn: 5\n\
  \ljgn: 2\n\
  \sjmn: drzm * dbpl\n\
  \sllz: 4\n\
  \pppw: cczh / lfqf\n\
  \lgvd: ljgn * ptdq\n\
  \drzm: hmdt - zczc\n\
  \hmdt: 32"

exOut = execute . buildTree $ readITree <$> lines exInp

main1 = do
  inp <- readFile "21_input.txt"
  let itrees = readITree <$> lines inp
  let res = execute . buildTree $ itrees
  print (showFFloat Nothing res "")
  print res

-- answer: 10037517593724



main2 = do
  inp <- readFile "21_input.txt"
  let itrees = readITree <$> lines inp
  let res = buildTree $ itrees
  print res


    -- ops are inverses of eachother
solve :: Tree -> Double
solve (Nil _ v) = v
solve (Branch n op a b) = (readOp op) (solve a) (solve b)
  where
    human = n == "humn"
    root = n == "root"
