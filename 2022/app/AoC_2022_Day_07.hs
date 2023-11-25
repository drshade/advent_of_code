module AoC_2022_Day_07 where

import           Control.Monad (void)
import           Data.List     (sort)
import           Data.Map      as Map (Map, empty, insert, lookup, toList)
import           Data.Maybe    (fromMaybe)
import           Text.Parsec
import           Utils         (read_data)

type Path = String

type MyParser a = Parsec String ([Path], Map Path Int) a

flatpath :: [Path] -> Path
flatpath = foldr (++) ""

inc_size :: [Path] -> Int -> Map Path Int -> Map Path Int
inc_size path@(_:ps) v m =
  let cursize = fromMaybe 0 $ Map.lookup (flatpath path) m
   in inc_size ps v (Map.insert (flatpath path) (cursize + v) m)
inc_size _ _ m = m

cd :: MyParser ()
cd = do
  void $ string "cd "
  arg <- many1 (letter <|> char '.')
  modifyState
    (\(path, sizes) ->
       case arg of
         ".." -> (tail path, sizes)
         _    -> (arg : path, sizes))

file :: MyParser ()
file = do
  size <- many1 digit
  void $ spaces
  _name <- many1 (letter <|> char '.')
  modifyState (\(path, sizes) -> (path, inc_size path (read size) sizes))

dir :: MyParser ()
dir = void $ string "dir " >> many1 letter

hell_parse :: MyParser (Map Path Int)
hell_parse = do
  void $ string "$ cd /" >> endOfLine
  many
    (string "$ " >>
     choice
       [ (string "ls" >> endOfLine >>
          void (many ((file <|> dir) >> (optional endOfLine))))
       , (cd >> void endOfLine)
       ]) >>
    eof
  (_, sizes) <- getState
  return $ sizes

run :: IO ()
run = do
  real_vals <- read_data id "data/AoC_2022_Day_07"
  case runParser hell_parse (["/"], empty) "(input)" real_vals of
    Left err ->
      putStrLn $ "A terribly unfortunate parsing error: " ++ (show err)
    Right result -> do
      putStrLn $
        "Day7 Q1  => " ++
        (show $ sum $ filter (<= 100000) $ snd <$> toList result)
      putStrLn $
        "     Q2  => " ++
        (show $
         head $
         sort $
         filter (\e -> e >= 30000000 - (70000000 - 43837783)) $
         snd <$> toList result)
