module Play where

import           Handy

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, modify, runStateT)
import           Data.Functor           (($>))
import qualified Data.Map               as Map
import           Text.Megaparsec        hiding (State, parse)
import           Text.Megaparsec.Char

data Action = Toggle | Off | On deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type Block = (Coord, Coord)

type MyParser a = Parser (StateT (Map.Map Action Int) IO) a

parser :: MyParser [(Action, Block)]
parser = do
    many entry
    where
        entry :: MyParser (Action, Block)
        entry = do
            (,) <$> action <* space
                <*> ((,) <$> coord <* string " through "
                         <*> coord <* optional newline)

        action :: MyParser Action
        action = do
            act <- choice
                    [ try $ string "toggle" $> Toggle
                    , try $ string "turn on" $> Off
                    , try $ string "turn off" $> On
                    ]

            -- update the state
            modify $ Map.insertWith (+) act 1

            -- Do some IO's even!
            liftIO $ putStrLn "dog!!"

            pure act
        coord :: MyParser Coord
        coord =
            (,) <$> (read <$> some digitChar <* char ',')
                <*> (read <$> some digitChar)

runMyParser :: IO ()
runMyParser = do
    input <- puzzle Main 2015 6
    let p = parse parser input
    let x = runStateT p Map.empty
    (result, state) <- x

    print result
    print state

    pure ()

parser' :: Parser' [(Int, Int)]
parser' = do
    many $ (,) <$> (read <$> some digitChar <* space)
               <*> (read <$> some digitChar <* optional newline)
