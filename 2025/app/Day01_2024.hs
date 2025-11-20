module Day01_2024 where

import qualified Data.Map             as Map
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec      (some)
import           Text.Megaparsec.Char (digitChar, newline, space)

parser :: Parser' [(Int, Int)]
parser = some $ (,) <$> (read <$> some digitChar) <* space
                    <*> (read <$> some digitChar) <* newline

part1 :: IO Int
part1 = do
    input <- parse' parser <$> puzzle Main 2024 1
    let matched = zip (sort $ fst <$> input) (sort $ snd <$> input)
    pure $ sum $ (\(e1, e2) -> abs $ e1 - e2) <$> matched

part2 :: IO Int
part2 = do
    input <- parse' parser <$> puzzle Main 2024 1
    let freqmap = foldr ((\e acc -> Map.insertWith (+) e 1 acc) . snd) Map.empty input
    pure $ foldr ((\e acc -> fromMaybe 0 (Map.lookup e freqmap) * e + acc) . fst) 0 input
