
module Day02 where

import           Data.List.Split      (chunksOf)
import           Handy
import           Prelude              hiding (empty, insert, some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

input :: Parser' [(Int, Int)]
input = some $ (,) <$> (num <* char '-') <*> num <* optional (char ',')

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2025 2
    pure $ sum $ foldr (\(start, end) ->
        (++) $ filter (\item ->
                let (one, two) = splitAt (length (show item) `div` 2) (show item)
                 in one == two
            ) [start..end]
        ) [] values

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2025 2
    pure $ sum $ foldr (\(start, end) ->
        (++) $ filter (\item ->
                any (\divisor -> allEqual $ chunksOf divisor (show item))
                    (divisors (length $ show item))
            ) [start..end]
        ) [] values
