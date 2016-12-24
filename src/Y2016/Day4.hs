module Y2016.Day4 where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char

-- "aaaaa-bbb-z-y-x-123[abxyz]" -> ("aaaaa-bbb-z-y-x", 123, "abxyz")
parse :: String -> (String, Int, String)
parse str = (encryptedNames, sectorId, checksum)
  where
    (lhs, rhs) = span (/= '[') str
    encryptedNames = init $ filter (`elem` ['a' .. 'z'] ++ ['-']) lhs
    sectorId = read $ filter (`elem` ['0' .. '9']) lhs
    checksum = (init . tail) rhs

countLetters :: String -> Map.Map Char Int
countLetters str =
  foldr
    (\ch map ->
        case Map.lookup ch map of
          Just val -> Map.insert ch (val + 1) map
          Nothing -> Map.insert ch 1 map)
    Map.empty
    str

sortLetters :: Map.Map Char Int -> [(Char, Int)]
sortLetters letterToCount = List.sortBy sortF $ Map.toList letterToCount
  where
    sortF (l1, c1) (l2, c2) = (compare c2 c1) `mappend` (compare l1 l2)

isRoomReal :: String -> (Bool, Int)
isRoomReal str = (top5 == checksum, sectorId)
  where
    (encryptedNames, sectorId, checksum) = parse str
    sorted = sortLetters $ countLetters $ filter (/= '-') encryptedNames
    top5 = map fst $ take 5 sorted

sumOfSectors :: [String] -> Int
sumOfSectors lines =
  let xs = map isRoomReal lines
  in foldr
       (\(bool, sector) sum ->
           if bool
             then sum + sector
             else sum)
       0
       xs

-- Given a char and an int X, shift the char to the right by X.
shiftCipher :: Char -> Int -> Char
shiftCipher char amount =
  Char.chr $ (Char.ord char - ordA + actualAmount) `mod` 26 + ordA
  where
    actualAmount = mod amount 26
    ordA = Char.ord 'a'

-- Given an input of "qzmt-zixmtkozy-ivhz-343[abcdef]", return its real name
-- "very encrypted name" and its sectorId 343
findRealName :: String -> (String, Int)
findRealName str = (realName, sectorId)
  where
    (encryptedNames, sectorId, checksum) = parse str
    realName =
      map
        (\l ->
            if l == '-'
              then ' '
              else shiftCipher l sectorId)
        encryptedNames

findSectorIdOfNorthPole :: [String] -> Int
findSectorIdOfNorthPole lines = snd $ head xs
  where
    pairs = map findRealName lines
    xs = filter (\(str, sectorId) -> isNorthPole str) pairs
    isNorthPole str = str == "northpole object storage"
