{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack, unpack)

part1 :: Int
part1 = length $ takeWhile ((/= "00000") . take 5) $ calc <$> [0 ..]
  where
    calc v = unpack $ encode $ hash (pack ("ckczppom" ++ show v))

part2 :: Int
part2 = length $ takeWhile ((/= "000000") . take 6) $ calc <$> [0 ..]
  where
    calc = unpack . encode . hash . pack . ("ckczppom" <>) . show
