module Y2016.Day1 where

import Data.List.Split

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

getMoves :: [String] -> [Move]
getMoves = map (\(dir:dis) -> Move (getDirection dir) (read dis :: Int))

calculateBlocks :: String -> Coordinate
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
