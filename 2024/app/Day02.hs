module Day02 where
import           Handy
import           Text.Parsec (char, digit, many1, newline, sepBy1)

parser :: Parser [[Int]]
parser = many1 (report <* newline)
    where report = (read <$> many1 digit) `sepBy1` char ' '

valid :: [Int] -> Bool
valid xs =
    let diffs = zipWith (-) xs (drop 1 xs)
        diffs' = if all (< 0) diffs then negate <$> diffs else diffs
     in all (\i -> i > 0 && i <= 3) diffs'

valid' :: [Int] -> Bool
valid' xs = any (== True) $ valid <$> combinations
    -- Generate all combinations of lists where one element is removed
    where combinations = [ take i xs ++ drop (i + 1) xs | i <- [0..length xs] ]

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 2
    pure $ length $ filter (== True) $ valid <$> input

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 2
    pure $ length $ filter (== True) $ valid' <$> input
