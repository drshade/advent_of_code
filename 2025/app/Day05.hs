
module Day05 where

import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' ([(Int,Int)], [Int])
input = (,) <$> (some ((,) <$> (num <* char '-') <*> num <* newline) <* newline)
            <*> some (num <* newline)

part1 :: IO Int
part1 = do
    (ranges, items) <- parse' input <$> puzzle Main 2025 5
    pure $ foldr (\item -> -- if within any range
               if any (\(rs, re) -> item >= rs && item <= re) ranges then (+ 1) else (+ 0)
           ) 0 items

merge :: Ord a => [(a, a)] -> [(a, a)] -- merge ranges together (list must be sorted)
merge [] = []
merge [x] = [x]                        -- vvv if range start overlaps, merge it
merge (r1@(s1,e1):r2@(s2,e2):rs) | e1 >= s2  = merge $ (s1, max e1 e2) : rs
                                 | otherwise = r1 : merge (r2 : rs)

part2 :: IO Int
part2 = do
    (ranges,_) <- parse' input <$> puzzle Main 2025 5
    pure $ foldr (\(s,e) -> (+) (e - s + 1)) 0 (merge $ sort ranges)
