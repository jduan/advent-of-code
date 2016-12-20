module Y2016.Day3 where

-- Check if 3 integers can form a valid triangle
isTriangle
  :: (Ord a, Num a)
  => a -> a -> a -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

-- Given a list of list of ints, count how many valid triangles there are.
-- Example input:
-- [
--   ["101", "301", "501"],
--   ["102", "302", "502"],
-- ]
countValidTriangles :: [[String]] -> Int
countValidTriangles lines = sum counts
  where
    list_of_ints = map to_ints lines
    to_ints :: [String] -> [Int]
    to_ints = map (\word -> read word :: Int)
    counts :: [Int]
    counts =
      map
        (\[a, b, c] ->
            if isTriangle a b c
              then 1
              else 0)
        list_of_ints
