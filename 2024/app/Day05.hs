module Day05 where
import           Data.List   (sortBy)
import           Handy
import           Text.Parsec (char, digit, many1, newline, sepBy1)

parser :: Parser ([(Int, Int)], [[Int]])
parser = (,) <$> many1 ((,) <$> (page <* char '|') <*> (page <* newline)) <* newline
             <*> many1 ((page `sepBy1` char ',') <* newline)
    where page = read <$> many1 digit

sort :: [(Int, Int)] -> [Int] -> [Int]
sort rules = sortBy (\a b -> if (a, b) `elem` rules then LT else GT)

mid :: [Int] -> Int
mid xs = xs !! (length xs `div` 2)

part1 :: IO Int
part1 = do
    (rules, pages) <- parse parser <$> getInput Main 2024 5
    pure $ sum $ mid <$> filter (\p -> sort rules p == p) pages

part2 :: IO Int
part2 = do
    (rules, pages) <- parse parser <$> getInput Main 2024 5
    pure $ sum $ mid . sort rules <$> filter (\ps -> sort rules ps /= ps) pages
