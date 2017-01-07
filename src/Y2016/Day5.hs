{-# LANGUAGE OverloadedStrings #-}

module Y2016.Day5 where

import qualified Data.Map as Map
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.MD5 (md5)
import Data.ByteString.Char8 (pack)

-- initialDoorId = "abc"
initialDoorId = "ffykfhsq"

calculateMD5 :: Int -> String
calculateMD5 n = show $ md5 doorId
  where
    doorId =
      LB.concat [initialDoorId, Builder.toLazyByteString (Builder.intDec n)]

calculateHashes :: [String]
calculateHashes =
  filter (\hash -> take 5 hash == "00000") $ map calculateMD5 [1 ..]

password :: String
password = map (!! 5) $ take 8 calculateHashes

-- Return a list of "positions" and "secrets", such as
-- [(6,'a'),(6,'c'),(9,'7')].
getPositionsAndSecrets :: [(Int, Char)]
getPositionsAndSecrets = map mapToInt $ filter validPosition $ map f calculateHashes
  where
    f :: String -> (Char, Char)
    f s = (s !! 5, s !! 6)
    validPosition (pos, _) = pos `elem` ['0' .. '7']
    charToInt char = read [char] :: Int
    mapToInt (pos, secret) = (charToInt pos, secret)

-- Given a 5k
password2 pairs = toList $ helper Map.empty pairs
  where
    helper m ((pos, secret):xs) =
      if Map.size m == 8
        then m
        else helper (fillMap m pos secret) xs
    toList m = map snd $ Map.toAscList m
    fillMap m pos secret =
      case Map.lookup pos m of
        Just value -> m
        Nothing -> Map.insert pos secret m
