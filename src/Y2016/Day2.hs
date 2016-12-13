module Y2016.Day2 where

data Direction
  = L
  | R
  | U
  | D
  deriving (Show, Eq)

mkDirection 'U' = U
mkDirection 'D' = D
mkDirection 'L' = L
mkDirection 'R' = R
mkDirection _ = error "Invalid direction!"

move
  :: (Num a, Num t, Eq a)
  => a -> Direction -> t
move 1 R = 2
move 1 D = 4
move 1 _ = 1
move 2 L = 1
move 2 R = 3
move 2 D = 5
move 2 U = 2
move 3 L = 2
move 3 D = 6
move 3 _ = 3
move 4 U = 1
move 4 D = 7
move 4 L = 4
move 4 R = 5
move 5 U = 2
move 5 D = 8
move 5 L = 4
move 5 R = 6
move 6 U = 3
move 6 D = 9
move 6 L = 5
move 6 R = 6
move 7 U = 4
move 7 R = 8
move 7 _ = 7
move 8 U = 5
move 8 D = 8
move 8 L = 7
move 8 R = 9
move 9 U = 6
move 9 L = 8
move 9 _ = 9

toDirections :: String -> [Direction]
toDirections = map mkDirection

makeMoves start input = foldl move start directions
  where
    directions = toDirections input
