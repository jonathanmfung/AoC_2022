import Data.Char

type Mark = Char

readMark :: Char -> Mark
readMark c = case c of
               'S' -> 'a'
               'E' -> '~'
               n -> n

canMove :: Mark -> Mark -> Bool
canMove a b = (succ a) >= b

finished :: Mark -> Bool
finished = (== '~')

inp = "Sabqponm\n\
      \abcryxxl\n\
      \accszExk\n\
      \acctuvwj\n\
      \abdefghi"


data Dir = Up | Rt | Dn | Lt | Null
