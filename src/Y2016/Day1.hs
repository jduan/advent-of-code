module Y2016.Day1 where

import Data.List.Split

input =
  "L3, R2, L5, R1, L1, L2, L2, R1, R5, R1, L1, L2, R2, R4, L4, L3, L3, R5, L1, R3, L5, L2, R4, L5, R4, R2, L2, L1, R1, L3, L3, R2, R1, L4, L1, L1, R4, R5, R1, L2, L1, R188, R4, L3, R54, L4, R4, R74, R2, L4, R185, R1, R3, R5, L2, L3, R1, L1, L3, R3, R2, L3, L4, R1, L3, L5, L2, R2, L1, R2, R1, L4, R5, R4, L5, L5, L4, R5, R4, L5, L3, R4, R1, L5, L4, L3, R5, L5, L2, L4, R4, R4, R2, L1, L3, L2, R5, R4, L5, R1, R2, R5, L2, R4, R5, L2, L3, R3, L4, R3, L2, R1, R4, L5, R1, L5, L3, R4, L2, L2, L5, L5, R5, R2, L5, R1, L3, L2, L2, R3, L3, L4, R2, R3, L1, R2, L5, L3, R4, L4, R4, R3, L3, R1, L3, R5, L5, R1, R5, R3, L1"

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

nextDirection :: Direction -> Turn -> Direction
nextDirection North L = West
nextDirection North R = East
nextDirection South L = East
nextDirection South R = West
nextDirection West L = South
nextDirection West R = North
nextDirection East L = North
nextDirection East R = South

data Turn
  = L
  | R
  deriving (Show, Eq)

type Distance = Int

type X = Int

type Y = Int

data Coordinate =
  Coordinate X
             Y
  deriving (Show, Eq)

-- Coordinate And Direction
data CAD =
  CAD Coordinate
      Direction

blocksAway :: Coordinate -> Int
blocksAway (Coordinate x y) = abs x + abs y

getDirection 'L' = L
getDirection 'R' = R
getDirection _ = error "Invalid turn!"

data Move =
  Move Turn
       Distance
  deriving (Show, Eq)

getMoves = map (\(dir:dis) -> Move (getDirection dir) (read dis :: Int))

calculateBlocks line =
  let (CAD coord dir) = calculate
  in coord
  where
    parts = splitOn ", " line
    moves = reverse $ getMoves parts
    calculate =
      foldr
        (\(Move turn distance) (CAD (Coordinate x y) direction) ->
            let nextDir = nextDirection direction turn
            in case nextDir of
                 North -> CAD (Coordinate x (y + distance)) nextDir
                 South -> CAD (Coordinate x (y - distance)) nextDir
                 East -> CAD (Coordinate (x + distance) y) nextDir
                 West -> CAD (Coordinate (x - distance) y) nextDir)
        (CAD (Coordinate 0 0) North)
        moves

-- part two
data Segment =
  Segment Coordinate
          Coordinate

-- If two coordinates overlap, return the intersection. Otherwise, return
-- nothing.
areSegmentsOverlapped :: Segment -> Segment -> Maybe Coordinate
areSegmentsOverlapped (Segment (Coordinate x1 y1) (Coordinate x2 y2)) (Segment (Coordinate x3 y3) (Coordinate x4 y4)) =
  Nothing
-- main :: IO ()
-- main =
--   let coord = calculateBlocks input
--   in print $ blocksAway coord
