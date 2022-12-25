{-# LANGUAGE LambdaCase #-}
import Data.List (null)
import Data.Either

example =
  [ "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  ]

type File = String

data Command = CDFile File | CDUp | CDRoot | LS
  deriving (Show, Eq)

data Output = Dir File | Size Int File
  deriving (Eq)

instance Show Output where
  show (Dir f) = show f ++ " (dir)"
  show (Size i f) = show f ++ " (file, size=" ++ show i ++ ")"

parseCommand :: [String] -> Command
parseCommand xs = case cmd of
  "ls" -> LS
  "cd" -> case arg of
    ".." -> CDUp
    "/" -> CDRoot
    _ -> CDFile arg
  where
    body = tail xs
    cmd = head body
    arg = last body

parseOutput :: [String] -> Output
parseOutput xs = case head xs of
  "dir" -> Dir $ last xs
  _ -> Size (read $ head xs) (last xs)

parseLine :: String -> Either Command Output
parseLine xs =
  if head ws == "$"
    then Left $ parseCommand ws
    else Right $ parseOutput ws
  where
    ws = words xs

data FS a = Direc a [FS a] | Files [a] | Nil
instance Show a => Show (FS a) where
  show (Direc a fs) = "- " ++ show a ++ "\n" ++ show fs
  show (Files as) = show as

type FileSystem = FS Output

parseFileSystem :: [[Either Command Output]] -> FileSystem
parseFileSystem xs = foldl go (Files [Dir ""]) xs
  where
--    go :: [[Either Command Output]] -> File -> FileSystem
    go :: FileSystem -> [Either Command Output] -> FileSystem
    go = \cases
           dir@(_) x@(Left CDRoot) -> go (Direc (Dir "/") [Nil])



-- group every LS with its output,
-- then left with only Commands

foo :: Eq a => [a] -> a -> [[a]]
foo xs match = tail $ go xs [] match
  where
    go :: Eq a => [a] -> [a] -> a -> [[a]]
    go (x : xs) remain m
      | x == m = [remain] ++ go (xs) [x] m
      | length (x : xs) == 1 = [remain ++ [x]]
      | otherwise = go (xs) (remain ++ [x]) m

group :: [Either Command Output] -> [[Either Command Output]]
group xs = tail $ go xs []
  where
    go :: [Either Command Output] -> [Either Command Output]
        -> [[Either Command Output]]
    go (x : xs) remain
      | isLeft x = [remain] ++ go (xs) [x]
      | length (x : xs) == 1 = [remain ++ [x]]
      | otherwise = go (xs) (remain ++ [x])

test = [1, 0, 0, 0, 1, 1, 0, 0, 1, 0]

res = [[1, 0, 0, 0], [1, 0, 0], [1, 0]]
