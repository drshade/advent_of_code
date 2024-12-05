module Day05 where
import           Data.List   (sortBy)
import           Handy
import           Text.Parsec (char, digit, many1, newline, optional, sepBy1)

type Rule = (Int, Int)
type Pages = [Int]

parser :: Parser ([Rule], [Pages])
parser = do
    pairs <- many1 $ (,) <$> (page <* char '|') <*> (page <* newline)
    _ <- newline
    pages <- many1 $ (page `sepBy1` char ',') <* optional newline
    pure (pairs, pages)
    where page = read <$> many1 digit

sort :: [Rule] -> [Int] -> [Int]
sort rules = sortBy (\a b -> if (a, b) `elem` rules then LT else GT)

mid :: [Int] -> Int
mid options = options !! (length options `div` 2)

part1 :: IO Int
part1 = do
    (rules, pages) <- parse parser <$> getInput Main 2024 5
    pure $ sum $ mid <$> filter (\p -> sort rules p == p) pages

part2 :: IO Int
part2 = do
    (rules, pages) <- parse parser <$> getInput Main 2024 5
    pure $ sum $ mid . sort rules <$> filter (\ps -> sort rules ps /= ps) pages
