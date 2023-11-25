module AoC_2021_Day_02 where

import           System.IO (IOMode (ReadMode), hGetContents, openFile)

test_vals :: [String]
test_vals =
  [ "forward"
  , "5"
  , "down"
  , "5"
  , "forward"
  , "8"
  , "up"
  , "3"
  , "down"
  , "8"
  , "forward"
  , "2"
  ]

-- Yes this is nuts, but look how cool it is!!
-- (also i remember there was a better way to do this, but i've forgotten)
-- basically a double fmap, e.g. for fmapping over IO [Int] / (IO (Array Int))
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) f fga = (\gb -> f <$> gb) <$> fga

read_words_as :: (String -> a) -> String -> IO [a]
read_words_as f filename = do
  f <$$> words <$> (openFile filename ReadMode >>= hGetContents)
  -- ^^ See???

track_aim :: Int -> Int -> Int -> [String] -> Int
track_aim aim h d ("forward":x:xs) =
  track_aim aim (h + read x) (d + (read x) * aim) xs
track_aim aim h d ("down":x:xs) = track_aim (aim + read x) h d xs
track_aim aim h d ("up":x:xs) = track_aim (aim - read x) h d xs
track_aim _ h d _ = h * d

track :: Int -> Int -> [String] -> Int
track h d ("forward":x:xs) = track (h + read x) d xs
track h d ("down":x:xs)    = track h (d + read x) xs
track h d ("up":x:xs)      = track h (d - read x) xs
track h d _                = h * d

run :: IO ()
run = do
  real_vals <- read_words_as id "data/AoC_2021_Day_02"
  putStrLn $ "Day2 Q1 => " ++ show (track 0 0 real_vals)
  putStrLn $ "Day2 Q2 => " ++ show (track_aim 0 0 0 real_vals)
