module Y2016.Day3 where

-- Check if 3 integers can form a valid triangle
isTriangle
  :: (Ord a, Num a)
  => a -> a -> a -> Bool
isTriangle a b c = a + b > c && a + c > b && b + c > a

toInts :: [String] -> [Int]
toInts = map (\word -> read word :: Int)

-- Given a list of list of ints, count how many valid triangles there are.
-- Example input:
-- [
--   ["101", "301", "501"],
--   ["102", "302", "502"],
-- ]
countValidTriangles :: [[String]] -> Int
countValidTriangles lines = sum counts
  where
    listOfInts = map toInts lines
    counts :: [Int]
    counts =
      map
        (\[a, b, c] ->
            if isTriangle a b c
              then 1
              else 0)
        listOfInts

-- Part 2
countValidTriangles2 :: [[String]] -> Int
countValidTriangles2 lines = count listOfInts
  where
    listOfInts = map toInts lines
    count (line1:line2:line3:rest) = count3Lines line1 line2 line3 + count rest
    count [] = 0
    count3Lines [a1, b1, c1] [a2, b2, c2] [a3, b3, c3] =
      sum $
      map
        (\bool ->
            if bool
              then 1
              else 0)
        bools
      where
        bools = [isTriangle a1 a2 a3, isTriangle b1 b2 b3, isTriangle c1 c2 c3]
