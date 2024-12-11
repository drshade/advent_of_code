module Day11 where
import           Data.MemoTrie (memoFix)
import           Handy
import           Text.Parsec   (char, digit, many1, sepBy)

parser :: Parser [Int]
parser = (read <$> many1 digit) `sepBy` char ' '

type Memoize f = f -> f

count :: Int -> Memoize ((Int, Int) -> Int)
count maxDepth recurse (depth, val) =
    if depth == maxDepth
        then 1
        else
            let strval = show val
            in case val of
                0 -> recurse (depth + 1, 1)
                _ | even $ length strval ->
                    let (left, right) = splitAt (length strval `div` 2) strval
                    in recurse (depth + 1, read left) + recurse (depth + 1, read right)
                v -> recurse (depth + 1, v * 2024)

count'memo :: Int -> (Int, Int) -> Int
count'memo maxD = memoFix (count maxD)

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 11
    pure $ sum $ (\i -> count'memo 25 (0, i)) <$> input

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 11
    pure $ sum $ (\i -> count'memo 75 (0, i)) <$> input
