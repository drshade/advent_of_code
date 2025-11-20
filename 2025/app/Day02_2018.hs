{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day02_2018 where

import qualified Data.Map             as Map
import           Handy
import           Prelude              hiding (empty, insert, some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

input :: Parser' [String]
input = some $ some letterChar <* optional newline

part1 :: IO Int
part1 = do -- filter for number of chars in a frequency map `Map Char Integer`
    values <- parse' input <$> puzzle Main 2018 2
    pure $ (*) (length $ filter ([] /=) (Map.elems . Map.filter (== 2) . freqmap <$> values))
               (length $ filter ([] /=) (Map.elems . Map.filter (== 3) . freqmap <$> values))
    where freqmap = foldr (\c m -> Map.insertWith (const (+ 1)) c 1 m) Map.empty

part2 :: IO String
part2 = do
    values <- parse' input <$> puzzle Main 2018 2
    let combinations = (,) <$> values <*> values
    pure $ head                                 -- return the first
         $ (\(c, s) -> delete (head c) s)       -- removing the different char
            <$> filter ((== 1) . length . fst)  -- where there is only 1 difference
                       (dif <$> combinations)   -- in each combination pair
    where dif :: (String, String) -> ([Char], String)
          dif (s1, s2) = (,) (zip s1 s2 >>= (\(a, b) -> ([a | a /= b]))) s1

