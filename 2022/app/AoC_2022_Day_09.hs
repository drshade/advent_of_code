module AoC_2022_Day_09 where

import           Data.Set    as Set (fromList, member)
import           Text.Parsec
import           Utils       (div_round, read_data)

type Coord = (Int, Int)

type Rope = [Coord]

trace_head :: Coord -> [(Char, Int)] -> [Coord]
trace_head (r, c) ((_, 0):rest) = trace_head (r, c) rest
trace_head (r, c) ((dir, cnt):rest)
  | dir == 'R' = move 0 1
  | dir == 'L' = move 0 (-1)
  | dir == 'U' = move (-1) 0
  | dir == 'D' = move 1 0
  | otherwise = []
  where
    move rd cd =
      (r + rd, c + cd) : (trace_head (r + rd, c + cd) $ (dir, cnt - 1) : rest)
trace_head _ _ = []

is_adjacent :: Coord -> Coord -> Bool
is_adjacent (hr, hc) (tr, tc) =
  member (tr, tc) $
  Set.fromList [(hr + ro, hc + co) | ro <- [(-1) .. 1], co <- [(-1) .. 1]]

nudge_toward :: Coord -> Coord -> Coord
nudge_toward (xr, xc) (yr, yc) = (div_round xr yr, div_round xc yc)

pull :: Rope -> Rope
pull (hd:tl:tls) =
  if hd `is_adjacent` tl
    then hd : pull (tl : tls)
    else hd : pull (tl `nudge_toward` hd : tls)
pull a = a

trace_tail :: Int -> Rope -> [Coord]
trace_tail rope_length head_moves =
  snd $
  foldl
    (\(rope, mem) newhead -> (pull $ newhead : (drop 1 rope), last rope : mem))
    ([(0, 0) | _ <- [1 .. rope_length]], [])
    head_moves

parseinput :: String -> Either ParseError [(Char, Int)]
parseinput =
  let parser =
        many $ do
          direction <- letter
          number <- space >> many digit
          _ <- optional endOfLine
          return (direction, read number)
   in runParser parser () "(input)"

run :: IO ()
run = do
  real <- read_data (id) "data/AoC_2022_Day_09"
  case parseinput real of
    Left err ->
      putStrLn $ "A terribly unfortunate parsing error: " ++ (show err)
    Right output -> do
      let head_moves = trace_head (0, 0) output
      putStrLn $
        "Day9 Q1  => " ++
        (show $ 1 + (length $ Set.fromList $ trace_tail 2 head_moves))
      putStrLn $
        "     Q2  => " ++
        (show $ length $ Set.fromList $ trace_tail 10 head_moves)
