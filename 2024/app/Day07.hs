module Day07 where
import           Handy
import           Text.Parsec (char, digit, many1, newline, optional, sepBy1,
                              string)

parser :: Parser [(Int, [Int])]
parser = many1 (entry <* optional newline)
    where entry = (,) <$> val <* string ": "
                      <*> val `sepBy1` char ' '
          val = read <$> many1 digit

solve :: (Int, [Int]) -> Int -> Bool
solve (solution, []) n = solution == n
solve (solution, x:xs) n = solve (solution, xs) (n + x) || solve (solution, xs) (n * x)

solve' :: (Int, [Int]) -> Int -> Bool
solve' (solution, []) n = solution == n
solve' (solution, x:xs) n = op (n + x) || op (n * x) || op (n ||| x)
    where op = solve' (solution, xs)
          (|||) a b = read $ show a ++ show b

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 7
    pure $ sum $ fst <$> filter (\(sol, xs) -> solve (sol, xs) 0) input

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 7
    pure $ sum $ fst <$> filter (\(sol, xs) -> solve' (sol, xs) 0) input
