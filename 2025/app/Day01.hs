{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day01 where

import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' [Int]
input = some $ (*) <$> choice [char 'L' $> (-1), char 'R' $> 1]
                   <*> (num <* optional newline)

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2025 1
    pure $ length
         $ filter (0 ==) -- scan around the dial, filtering only zeros
         $ scanl (\v -> (`mod` 100) . (+) v) 50 values

-- So horrible. Surely some better maths here!
zeroes :: Int -> Int -> Int
zeroes dial dir -- turning right, then just divide (rounds towards zero)
                | dir > 0                   = (dial + dir) `div` 100
                -- turning left, on zero its just divide
                | dir < 0 && dial == 0      = (-dir) `div` 100
                -- turning left past zero, then it's 1 plus (rounds towards -inf)
                | dir < 0 && (-dir) >= dial = 1 + ((-dir) - dial) `div` 100
                | otherwise                 = 0

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2025 1
    pure $ snd $ foldl -- continuously turn the dial and count up the clicks
        (\(dial, acc) dir -> (,) ((dial + dir) `mod` 100) (acc + zeroes dial dir)
        ) (50, 0) values
