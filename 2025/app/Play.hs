module Play where

import           Handy

import           Control.Monad          (foldM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, modify, runStateT)
import           Data.Functor           (($>))
import qualified Data.Map               as Map
import           Text.Megaparsec        hiding (State, parse)
import           Text.Megaparsec.Char

play :: [(Int, Int)]
play = do
    x <- [1,2,3,4]
    y <- [5,6,7,8]
    pure (x,y)


