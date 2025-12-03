{- HLINT ignore "Redundant <$>" -}
{- HLINT ignore "Functor law" -}

module Day02 where

import           Data.List.Split      (chunksOf)
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' [(Int, Int)]
input = some $ (,) <$> (num <* char '-') <*> num <* optional (char ',')

part1 :: IO Int
part1 = sum . foldr (\(start, end) ->
        (++) $ filter (\item ->
            let (one, two) = splitAt (length (show item) `div` 2) (show item)
             in one == two
            ) [start..end]
    ) [] <$> parse' input <$> puzzle Main 2025 2

part2 :: IO Int
part2 = do
    sum . foldr (\(start, end) ->
        (++) $ filter (\item ->
                any (\divisor -> allEqual $ chunksOf divisor (show item))
                    (divisors (length $ show item))
            ) [start..end]
        ) [] <$> parse' input <$> puzzle Main 2025 2
