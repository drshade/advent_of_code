module Day07 where
import           Debug.Trace (trace)
import           Handy
import           Text.Parsec (char, digit, many1, newline, optional, sepBy1,
                              string)

parser :: Parser [(Int, [Int])]
parser = many1 (p <* optional newline)
    where p = do
             result <- val <* string ": "
             options <- val `sepBy1` char ' '
             pure (result, options)
          val = read <$> many1 digit

solve :: (Int, [Int]) -> Int -> Bool
solve (solution, []) n = solution == n
solve (solution, x:xs) n = solve (solution, xs) (n + x) || solve (solution, xs) (n * x)

solve' :: (Int, [Int]) -> Int -> Bool
solve' (solution, []) n = solution == n
solve' (solution, x:xs) n =
    solve' (solution, xs) (n + x) || solve' (solution, xs) (n * x) || solve' (solution, xs) (glue n x)
    where glue a b = read $ show a ++ show b

part1 :: IO ()
part1 = do -- 1260333054159
    input <- parse parser <$> getInput Main 2024 7
    putStrLn $ show $ sum $ fst <$> filter (\(sol, xs) -> solve (sol, xs) 0) input

part2 :: IO ()
part2 = do -- 162042343638683
    input <- parse parser <$> getInput Main 2024 7
    putStrLn $ show $ sum $ fst <$> filter (\(sol, xs) -> solve' (sol, xs) 0) input
