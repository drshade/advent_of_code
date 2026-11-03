{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
module Day02_2019 where

import           Control.Lens
import           Handy
import           Prelude              hiding (some)
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char

-- We can't parse the codes into machine instructions too early, as
-- we will update them during processing - like an interpretter.
-- So we just deal with list of int for now... kinda sucks - but thats
-- the puzzle i guess!

input :: IO [Int]
input = parse' (num `sepBy` char ',' <* newline <* eof) <$> puzzle Main 2019 2

step :: Int -> State [Int] ()
step pc = do
    prog <- get
    case drop pc prog of          -- Lens get by index: `^?! ix n`
        (1:op1:op2:op3:_) -> do   -- Lens set by index: `& ix n .~ v`
                put $ prog & ix op3 .~ (prog ^?! ix op1 + prog ^?! ix op2)
                step $ pc + 4
        (2:op1:op2:op3:_) -> do
                put $ prog & ix op3 .~ (prog ^?! ix op1 * prog ^?! ix op2)
                step $ pc + 4
        (99:_) -> pure () -- prog ends!
        _ -> error "invalid opcode!"

part1, part2 :: IO Int
part1 = do
    prog <- input
    let result = execState (step 0) (prog & ix 1 .~ 12 & ix 2 .~ 2)
    pure (result ^?! ix 0)
part2 = do
    prog <- input -- run the program for every noun + verb, finding the one that matches
    let runs = [(n, v, execState (step 0) (prog & ix 1 .~ n & ix 2 .~ v)) | n <- [0..99], v <- [0..99]]
    let Just (n,v,_) = find (\(_,_,p) -> (p ^?! ix 0) == 19690720) runs
    pure $ 100 * n + v

