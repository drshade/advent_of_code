module Utils where

import           Control.Monad (join)
import           System.IO     (IOMode (ReadMode), hGetContents, openFile)

read_data :: (String -> a) -> String -> IO a
read_data f filename = do
  pure f <*> (openFile filename ReadMode >>= hGetContents)

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

dropLast :: (Eq t, Num t) => t -> [a] -> [a]
dropLast 0 xs = xs
dropLast n xs = dropLast (n - 1) $ init xs

trim :: String -> String
trim = unwords . words

insert :: [a] -> Int -> [a] -> [a]
insert es i l = take i l ++ es ++ drop (i + 1) l

(<$$>) :: (Functor g, Functor f) => (a -> b) -> g (f a) -> g (f b)
(<$$>) f gfa = (\fa -> f <$> fa) <$> gfa

infixl 8 <$$>

-- Always rounds closest to the second argument
div_round :: Int -> Int -> Int
div_round l r =
  if even total
    then total `div` 2
    else if l > r
           then (total - 1) `div` 2
           else (total + 1) `div` 2
  where
    total = l + r

(<$->) :: Monad m => (a -> m b) -> m a -> m b
(<$->) f ma = join $ f <$> ma

range :: Int -> Int -> [Int]
range a b =
  if a < b
    then [a .. b]
    else [a,pred a .. b]

-- Take a list and chunk it into smaller lists
-- e.g. chunk 2 [1,2,3,4] => [[1,2], [3,4]]
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size l =
  let first = take size l
   in [first] <> chunk size (drop size l)

-- My own cool monad combinator which runs the function until we
-- hit a predicate. This should probably be adjusted to not be specific
-- to [a] but rather t a, where t is a monoid i guess? (i am on a plane
-- so can't google ... )
takeUntilM :: Monad m => (b -> Bool) -> (a -> m b) -> [a] -> m ([b])
takeUntilM _ _ [] = pure mempty
takeUntilM predicate f (a:as) = do
  b <- f a
  if not $ predicate b
    then do
      r <- takeUntilM predicate f as
      pure $ b : r
    else pure mempty
