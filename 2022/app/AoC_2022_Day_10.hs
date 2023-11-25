module AoC_2022_Day_10 where

import           Data.List       (intercalate)
import           Data.List.Split (chunksOf)
import           Data.Set        (fromList, member)
import           Text.Parsec
import           Utils           (read_data)

data Op
  = NoOp
  | Addx Int

type Tape = [(Int -> Int)]

push :: Op -> Tape -> Tape
push NoOp tape     = id : tape
push (Addx v) tape = id : (\s -> s + v) : tape

execute :: Tape -> Int -> [Int]
execute (f:fs) v = f v : execute fs (f v)
execute _ _      = []

render_crt :: [Int] -> String
render_crt input =
  intercalate "\n" $
  chunksOf 40 $
  foldl (\output (x, i) -> output ++ punch x i) "" $ zip input [0 ..]
  where
    punch x i
      | (i `mod` 40) `member` fromList [x - 1 .. x + 1] = "X"
      | otherwise = " "

parseinput :: String -> Either ParseError [Op]
parseinput = runParser parser () "(input)"
  where
    parser =
      many $ do
        op <- parse_op
        _ <- optional endOfLine
        return op
    parse_op = do
      op <- many letter
      case op of
        "noop" -> return NoOp
        "addx" -> do
          number <- space >> many (digit <|> char '-')
          return (Addx $ read number)
        _ -> unexpected "Invalid op!"

run :: IO ()
run = do
  real <- read_data (id) "data/AoC_2022_Day_10"
  case parseinput real of
    Left err ->
      putStrLn $ "A terribly unfortunate parsing error: " ++ (show err)
    Right input -> do
      let program = id : (foldr push [] $ input)
      let program_output = execute program 1
      let steps = [i * (program_output !! (i - 1)) | i <- [20,60 .. 220]]
      putStrLn $ "Day10 Q1 => " ++ (show $ sum $ steps)
      putStrLn $ "      Q2 => \n" ++ render_crt program_output
