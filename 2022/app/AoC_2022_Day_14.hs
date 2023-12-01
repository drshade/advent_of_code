{-# LANGUAGE ScopedTypeVariables #-}

module AoC_2022_Day_14 where

import           Control.Applicative (Alternative ((<|>)))
import           Control.Monad.State (State, get, put, runState)
import           Data.List           as List (foldr, intercalate, length,
                                              maximum, minimum)
import           Data.Map            (Map)
import           Data.Map            as Map (empty, insert, keys, lookup)
import           Data.Maybe          (fromMaybe)
import           Text.Parsec         (Parsec, char, digit, many1, newline,
                                      optional, runParser, string)
import           Utils               (chunk, range, read_data, takeUntilM)

type Position = (Int, Int)

data Sim a =
  Sim
    { s_map :: Map Position a
    , s_def :: a
    }
  deriving (Show)

test :: String
test = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

parser :: Parsec String () [[Position]]
parser =
  let position = do
        x <- read <$> many1 digit
        _ <- char ','
        y <- read <$> many1 digit
        return (x, y)
      segment = many1 $ position <* (optional $ string " -> ")
   in many1 $ segment <* (optional newline)

get_pos :: Position -> State (Sim a) a
get_pos pos = do
  (Sim smap def) <- get
  pure $ fromMaybe def (Map.lookup pos smap)

put_pos :: Position -> a -> State (Sim a) ()
put_pos pos v = do
  state@(Sim smap _) <- get
  _ <- put $ state {s_map = (Map.insert pos v smap)}
  pure ()

put_pos_many :: [Position] -> a -> State (Sim a) ()
put_pos_many (a:as) v = do
  _ <- put_pos a v
  put_pos_many as v
put_pos_many _ _ = pure ()

dimensions :: State (Sim a) (Int, Int, Int, Int)
dimensions = do
  (Sim smap _) <- get
  let min_x = List.minimum $ (fst <$> Map.keys smap)
      min_y = 0
      max_x = List.maximum $ (fst <$> Map.keys smap)
      max_y = List.maximum $ (snd <$> Map.keys smap)
  pure $ (min_x, min_y, max_x, max_y)

print_sim :: State (Sim Object) String
print_sim = do
  (min_x, min_y, max_x, max_y) <- dimensions
  let all_xy = [(x, y) | y <- [min_y .. max_y], x <- [min_x .. max_x]]
  x <- mapM get_pos all_xy
  let print_object c =
        case c of
          Empty -> '_'
          Wall  -> '#'
          Sand  -> 'o'
  pure $
    intercalate "\n" $
    (\r -> print_object <$> r) <$> chunk (max_x - min_x + 1) x

run_sim :: a -> State (Sim a) b -> (b, Sim a)
run_sim def prog = runState prog $ Sim Map.empty def

-- From a list of points to a list of points along the snake path
snake :: [Position] -> [Position]
snake verts =
  let walk :: Position -> [Position] -> [Position]
      walk next [] = [next]
      walk (nx, ny) acc@((ax, ay):_) =
        reverse $ acc <> [(x, y) | x <- range ax nx, y <- range ay ny]
   in List.foldr walk [] verts

-- Takes the raw list of positions from the input file and loads them
-- into the State, while snaking out the pathways
load :: [[Position]] -> a -> State (Sim a) ()
load (p:ps) v = do
  put_pos_many (snake p) v
  load ps v
load _ _ = pure ()

data Object
  = Empty
  | Wall
  | Sand
  deriving (Eq)

data Fall
  = Settled
  | ThroughBottom
  | Blocked
  deriving (Eq)

-- The falling sand algorithm... looks terrible
fall :: Position -> Int -> Object -> State (Sim Object) Fall
fall (x, y) max_y v = do
  if y > max_y
    then pure ThroughBottom
    else do
      below <- get_pos (x, y + 1) -- Move down
      if below == Empty
        then fall (x, y + 1) max_y v
        else do
          left <- get_pos (x - 1, y + 1)
          if left == Empty
            then fall (x - 1, y + 1) max_y v
            else do
              right <- get_pos (x + 1, y + 1)
              if right == Empty
                then fall (x + 1, y + 1) max_y v
                else do
                  _ <- put_pos (x, y) v
                  pure Settled

-- The falling sand algorithm... looks terrible
fall' :: Position -> Position -> Int -> Object -> State (Sim Object) Fall
fall' origin (x, y) max_y v = do
  origin' <- get_pos origin
  if origin' /= Empty
    then pure Blocked
    else if y >= max_y
           then do
             _ <- put_pos (x, y) v
             pure Settled
           else do
             below <- get_pos (x, y + 1) -- Move down
             if below == Empty
               then fall' origin (x, y + 1) max_y v
               else do
                 left <- get_pos (x - 1, y + 1)
                 if left == Empty
                   then fall' origin (x - 1, y + 1) max_y v
                   else do
                     right <- get_pos (x + 1, y + 1)
                     if right == Empty
                       then fall' origin (x + 1, y + 1) max_y v
                       else do
                         _ <- put_pos (x, y) v
                         pure Settled

run :: IO ()
run = do
  real <- read_data (id) "data/AoC_2022_Day_14"
  case runParser parser () "(input)" real of
    Left err ->
      putStrLn $ "A terribly unfortunate parsing error: " ++ (show err)
    Right positions -> do
      let simulate_sand_prog_1 = do
            _ <- load positions Wall
            (_, _, _, max_y) <- dimensions
            result <-
              takeUntilM
                (== ThroughBottom)
                (const $ fall (500, 0) max_y Sand)
                [0 :: Int ..]
            let settled = List.length $ result
            -- print_sim
            pure $ "Day14 Q1 => " <> (show settled)
      let result1 = run_sim Empty simulate_sand_prog_1
      putStrLn $ fst result1
      let simulate_sand_prog_2 = do
            _ <- load positions Wall
            (_, _, _, max_y) <- dimensions
            result <-
              takeUntilM
                (== Blocked)
                (const $ fall' (500, 0) (500, 0) (max_y + 1) Sand)
                [0 :: Int ..]
            let settled = List.length $ result
            -- print_sim
            pure $ "Day14 Q2 => " <> (show settled)
      let result2 = run_sim Empty simulate_sand_prog_2
      putStrLn $ fst result2
      pure ()
  pure ()
