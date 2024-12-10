module Day09 where
import           Control.Monad.ST    (runST)
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector         as Vec
import qualified Data.Vector.Mutable as MVec
import           Handy
import           Text.Parsec         (Parsec, anyChar, digit, getState, many1,
                                      modifyState, newline, optionMaybe,
                                      optional)

type FileId = Int
data Block = File FileId | Free deriving (Show, Eq)

parser :: Parsec String (Int, Vec.Vector Block) (Int, Vec.Vector Block)
parser = do
    _ <- many1 (do
                    (nextId, vec) <- getState
                    size <- val
                    free <- optionMaybe val
                    _ <- modifyState $ const ( nextId + 1
                                             , vec
                                                Vec.++ Vec.replicate size (File nextId)
                                                Vec.++ Vec.replicate (fromMaybe 0 free) Free
                                             )
                    pure ()
                ) <* newline
    getState
    where val = read . (: []) <$> digit

compact :: Vec.Vector Block -> Vec.Vector Block
compact blocks =
    case Vec.findIndex (== Free) blocks of
        Nothing -> blocks
        Just index -> let (blocks', last') = fromMaybe (error "huh") $ Vec.unsnoc blocks
                       in compact $ blocks' Vec.// [(index, last')]

checksum :: Vec.Vector Block -> Int
checksum blocks = c $ zip [0..] $ Vec.toList blocks
    where
        c ((_index, File _id):xs) = _id * _index + c xs
        c ((_, Free):_)           = error "shouldn't?"
        c []                      = 0

type FileId' = Int
type Size = Int
data Block' = File' FileId Size | Free' Size deriving (Show, Eq)

parser' :: Parsec String (Int, [Block']) [Block']
parser' = do
    _ <- many1 (do
                    (nextId, vec) <- getState
                    size <- val
                    free <- optionMaybe val
                    _ <- modifyState $ const ( nextId + 1
                                             , Free' (fromMaybe 0 free) : File' nextId size : vec
                                             )
                    pure ()
                ) <* newline
    (_, blocks) <- getState
    pure $ reverse blocks
    where val = read . (: []) <$> digit

part1 :: IO Int
part1 = do
    putStrLn "parsing..."
    (_, blocks) <- parseWithState parser (0, Vec.empty) <$> getInput (Main) 2024 9
    putStrLn $ "parsed..."
    pure $ checksum $ compact blocks

part2 :: IO ()
part2 = do
    blocks <- parseWithState parser' (0, []) <$> getInput (Example 1) 2024 9
    putStrLn $ show blocks
    -- putStrLn $ show $ compact' blocks (reverse blocks) []
