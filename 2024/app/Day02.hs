module Day02 where
import           Handy
import           Text.Parsec (anyChar, many1)

parser :: Parser String
parser = many1 anyChar

part1 :: IO ()
part1 = do
    input <- parse parser <$> getInput Main 2024 2
    putStrLn $ show input
