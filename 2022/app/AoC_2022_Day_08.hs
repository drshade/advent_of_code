module AoC_2022_Day_08 where

import           Data.Matrix (Matrix, fromLists, getElem, mapPos, safeGet)
import           Data.Maybe  (isJust)
import           Utils       (read_data, (<$$>))

directions :: [(Int, Int)]
directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- Get an xray down a particular direction, returning all the trees
xray :: Matrix Int -> (Int, Int) -> (Int, Int) -> [Int]
xray mat (r, c) (ri, ci) =
  let options = (\i -> (r + (ri * i), c + (ci * i))) <$> [1 ..]
      valid = takeWhile (\(ro, co) -> isJust $ safeGet ro co mat) options
   in (\(ro, co) -> getElem ro co mat) <$> valid

-- Can I be seen from any angle
-- Send an xray in all directions and then OR them together
visible :: Matrix Int -> (Int, Int) -> Int -> Bool
visible mat pos v =
  foldr (||) False $ ((\dir -> all (< v) $ xray mat pos dir) <$> directions)

-- Calculate my scenery score
scenery_score :: Matrix Int -> (Int, Int) -> Int -> Int
scenery_score mat pos v =
  foldr (*) 1 $ (\dir -> calc $ xray mat pos dir) <$> directions
  where
    calc sight =
      if all (< v) sight
        then length sight
        else 1 + (length $ takeWhile (< v) sight)

run :: IO ()
run = do
  filedata <- read_data id "data/AoC_2022_Day_08"
  let trees = fromLists $ (\c -> read [c]) <$$> lines filedata
  let visibles = mapPos (visible trees) trees
  putStrLn $ "Day8 Q1  => " ++ (show $ sum $ fromEnum <$> visibles)
  let scenery_scores = mapPos (scenery_score trees) trees
  putStrLn $ "     Q1  => " ++ (show $ foldl max 0 scenery_scores)
