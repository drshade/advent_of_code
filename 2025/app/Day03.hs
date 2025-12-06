module Day03 where

import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: Parser' [[Int]]
input = some $ (some $ read . (:[]) <$> digitChar) <* optional newline

-- Find the index of the maximum value in a (non-empty) list
indexOfMax :: Ord a => [a] -> (Int, a)
indexOfMax inp = foldl (\old@(_, max') (idx, v) -> if v > max' then (idx, v) else old) (0, head inp) $ zip [0..] inp

part1 :: IO Int
part1 = do
    values <- parse' input <$> puzzle Main 2025 3
    pure $ sum $ (\inp ->
                    let (idx, big) = indexOfMax $ take (length inp - 1) inp -- drop the last
                        small = maximum $ drop (idx + 1) inp -- find the next biggest after index
                    in (big * 10) + small
                 ) <$> values

part2 :: IO Int
part2 = do
    values <- parse' input <$> puzzle Main 2025 3
    let results = (\inp ->
                reverse $ snd $ foldl (\(idx, acc) i ->
                    -- Hairy! Must drop enough from the right to make sure we don't overrun.
                    let (idx', biggest) = indexOfMax $ take (length inp - idx - (12 - i)) (drop (idx + 1) inp)
                    -- idx' is relative to the smaller list, so must add idx to get absolute position
                     in (idx + idx' + 1, biggest : acc)
                ) (-1, []) [0..11]
            ) <$> values
    pure $ sum $ foldl (\acc digit -> acc * 10 + digit) 0 <$> results
