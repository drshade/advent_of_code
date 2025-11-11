module Handy (Parser, Parser', parse, lift, puzzle, parse', Year, Day, PuzzleType(..)) where

import           Control.Monad              (unless)
import           Control.Monad.Identity     (Identity (runIdentity))
import           Control.Monad.Trans        (lift)
import qualified Data.ByteString.Char8      as Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (unpack)
import           Data.Functor               (void)
import           Data.Void                  (Void)
import           Network.HTTP.Client        (httpLbs, newManager, parseRequest,
                                             requestHeaders, responseBody,
                                             responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Directory           (createDirectory,
                                             doesDirectoryExist, doesFileExist)
import           System.IO                  (IOMode (ReadMode), hGetContents,
                                             openFile)
import           Text.Megaparsec            (ParsecT, runParserT)

-- Most practically useful Parser in any context
type Parser m a = ParsecT Void String m a

-- And a simpler version using Identity
type Parser' a = Parser Identity a

-- Run parser or die! Big-boy version
parse :: forall m a. Monad m => Parser m a -> String -> m a
parse parser input = do
    result <- runParserT parser "(input)" input
    case result of
        Left err -> error $ "A terribly unfortunate parsing error:\n" <> show err
        Right a  -> return a

-- Run parser or die! Identity version
parse' :: Parser' a -> String -> a
parse' parser input =
    case runIdentity (runParserT parser "(input)" input) of
        Left err -> error $ "A terribly unfortunate parsing error:\n" <> show err
        Right a  -> a

-- Get the puzzle input, either from disk, or from http first time
--
type Year = Int
type Day = Int
data PuzzleType = Main | Example Int

puzzle :: PuzzleType -> Year -> Day -> IO String
puzzle which year day = do
    -- Create directory and download file if it doesn't already exist
    void $ do
        doesDirectoryExist local_path >>= \exists -> unless exists (createDirectory local_path)
        doesFileExist (local_path <> local_file) >>= \exists -> unless exists downloadFile

    -- Return the contents
    openFile (local_path <> local_file) ReadMode >>= hGetContents

    where
        local_path = "data/"
        local_file = case which of
            Example n -> "input_" <> show year <> "_" <> show day <> "_example_" <> show n
            Main      -> "input_" <> show year <> "_" <> show day
        download_url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
        downloadFile :: IO ()
        downloadFile = do
            putStrLn $ "Downloading input for year " <> show year <> " day " <> show day <> " (will be cached)"
            cookie <- readFile "cookie.txt"
            req <- parseRequest download_url
            let req0 = req{requestHeaders = [(hCookie, Char8.pack cookie)]}
            manager <- newManager tlsManagerSettings
            resp <- httpLbs req0 manager
            if statusCode (responseStatus resp) /= 200
            then do
                let body :: String = LChar8.unpack $ responseBody resp
                error $ "Failed to download input for year " <> show year <> " day " <> show day <> " => " <> body
            else do
                let body :: String = LChar8.unpack $ responseBody resp
                writeFile (local_path <> local_file) body
                pure ()
