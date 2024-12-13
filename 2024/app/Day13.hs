module Day13 where
import           Data.Maybe  (catMaybes)
import           Handy
import           Text.Parsec (char, digit, many1, newline, optional, string)

type Button = (Int, Int)
type Prize = (Int, Int)

parser :: Parser [(Button, Button, Prize)]
parser = many1 $ (,,) <$> button 'A' <*> button 'B' <*> prize <* optional newline
    where button a = string "Button " *> char a *> string ": " *> coords '+' <* newline
          prize = string "Prize: " *> coords '=' <* newline
          coords symbol = (,) <$> (char 'X' *> char symbol *> val)
                              <*> (string ", Y" *> char symbol *> val)
          val = read <$> many1 digit

-- Thanks ChatGPT (implementation of gaussian elimination)
solve :: Button -> Button -> Prize -> Maybe (Int, Int)
solve (ax, ay) (bx, by) (px, py) =
    let det = ax * by - ay * bx  -- Determinant of A
    in if det == 0
       then Nothing  -- No solution if determinant is zero
       else let numX = px * by - py * bx
                numY = ax * py - ay * px
             in if numX `mod` det == 0 && numY `mod` det == 0  -- Ensure integer results
                    then Just (numX `div` det, numY `div` det)
                    else Nothing

cost :: (Int, Int) -> Int
cost (a, b) = (a * 3) + b

part1 :: IO Int
part1 = do
    input <- parse parser <$> getInput Main 2024 13
    pure $ sum $ cost <$> catMaybes ((\(a, b, prize) -> solve a b prize) <$> input)

part2 :: IO Int
part2 = do
    input <- parse parser <$> getInput Main 2024 13
    pure $ sum $ cost <$> catMaybes ((\(a, b, (px, py)) -> solve a b (px+10000000000000, py+10000000000000)) <$> input)
