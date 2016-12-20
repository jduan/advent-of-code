module Y2016.Day3 where

data Triangle =
  Triangle Int
           Int
           Int

isValidTriangle :: Triangle -> Bool
isValidTriangle (Triangle a b c) = a + b > c && a + c > b && b + c > a

-- Turn a list of strings into a Triangle
toTriangle :: [String] -> Triangle
toTriangle [a, b, c] = Triangle (read a :: Int) (read b :: Int) (read c :: Int)

-- Turn a list of list of strings into a list of Triangles
toTriangles = map toTriangle

-- Given a list of list of ints, count how many valid triangles there are.
-- Example input:
-- [
--   ["101", "301", "501"],
--   ["102", "302", "502"],
-- ]
countValidTriangles :: [[String]] -> Int
countValidTriangles lines = length $ filter isValidTriangle $ toTriangles lines

-- Part 2
countValidTriangles2 :: [[String]] -> Int
countValidTriangles2 lines = count $ toTriangles lines
  where
    count (tri1:tri2:tri3:rest) = count3Tris tri1 tri2 tri3 + count rest
    count [] = 0
    -- if the input isn't a multiple of 3, ignore the remaining lines
    count _ = 0
    count3Tris (Triangle a1 b1 c1) (Triangle a2 b2 c2) (Triangle a3 b3 c3) =
      length $ filter (== True) bools
      where
        bools =
          [ isValidTriangle (Triangle a1 a2 a3)
          , isValidTriangle (Triangle b1 b2 b3)
          , isValidTriangle (Triangle c1 c2 c3)
          ]

    
-- Transform the input first, then call countValidTriangles
countValidTriangles2' :: [[String]] -> Int
countValidTriangles2' lines = countValidTriangles $ transform lines
  where
    transform [] = []
    transform ([a1, b1, c1]:[a2, b2, c2]:[a3, b3, c3]:rest) =
      [a1, a2, a3] : [b1, b2, b3] : [c1, c2, c3] : transform rest
    -- if the input isn't a multiple of 3, ignore the remaining lines
    transform a = []
