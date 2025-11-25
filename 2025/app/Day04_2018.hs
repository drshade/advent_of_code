module Day04_2018 where

import qualified Data.Map             as Map
import           Handy
import           Prelude              hiding (empty, insert, some)
import           Text.Megaparsec      hiding (empty)
import           Text.Megaparsec.Char

type Timestamp = (Int, Int, Int, Int, Int)
data Action = Begin Int | Wakeup | Sleep deriving (Show)
data Entry = Entry Timestamp Action deriving (Show)

input :: Parser' [Entry]
input = some $ Entry <$> timestamp
                     <*> choice [ string "wakes up" $> Wakeup
                                , string "falls asleep" $> Sleep
                                , string "Guard " *> (Begin <$> (char '#' *> num)) <* string " begins shift"
                                ] <* optional newline
    where timestamp = (,,,,) <$> (char '[' *> num <* char '-')
                             <*> (num <* char '-')
                             <*> (num <* char ' ')
                             <*> (num <* char ':')
                             <*> (num <* string "] ")

part1 :: IO ()
part1 = do
    values <- parse' input <$> puzzle Main 2018 4
    print values
    pure ()

part2 :: IO ()
part2 = do
    values <- parse' input <$> puzzle Main 2018 4
    pure ()

