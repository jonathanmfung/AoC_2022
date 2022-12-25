import Data.List (nub)

testInp =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

testOut = [7, 5, 6, 10, 11] ++ [19, 23, 23, 29, 26]

verify = (startOfPacket <$> testInp) ++ (startOfMessage <$> testInp) == testOut

marker :: Int -> Int -> String -> Int
marker n l xs =
  if (length . nub $ take l xs) == l
    then n
    else marker (n + 1) l (tail xs)

startOfPacket :: String -> Int
startOfPacket = marker 4 4

main1 = do
  inp <- readFile "06_input.txt"
  let res = startOfPacket inp
  print res

-- answer: 1833

startOfMessage :: String -> Int
startOfMessage = marker 14 14

main2 = do
  inp <- readFile "06_input.txt"
  let res = startOfMessage inp
  print res
