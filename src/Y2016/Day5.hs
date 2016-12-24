{-# LANGUAGE OverloadedStrings #-}

module Y2016.Day5 where

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
