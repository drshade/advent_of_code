module Handy where

import qualified Data.ByteString.Char8      as Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (unpack)
import           Data.Functor               (void)
import           Data.String.Interpolate    (i)
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
import           Text.Parsec                (Parsec, runParser)

type Parser a = Parsec String () a

-- Run parser or die!
parse :: Parsec String () a -> String -> a
parse parser input =
  case runParser parser () "(input)" input of
    Left err -> error $ "A terribly unfortunate parsing error: " ++ (show err)
    Right a  -> a

-- Get the puzzle input, either from disk, or from http first time
--
type Year = Int
type Day = Int
data PuzzleType = Main | Example Int

getInput :: PuzzleType -> Year -> Day -> IO String
getInput which_input year day = do
  let local_path = "data/"
      local_file = case which_input of
        Example n -> [i|input_#{year}_#{day}_example_#{n}|]
        Main      -> [i|input_#{year}_#{day}|]
      download_url = [i|https://adventofcode.com/#{year}/day/#{day}/input|]
      downloadFile :: IO ()
      downloadFile = do
        putStrLn $
          [i|Downloading input for year #{year} day #{day} (will be cached)|]
        cookie <- readFile "cookie.txt"
        req <- parseRequest download_url
        let req0 = req{requestHeaders = [(hCookie, Char8.pack cookie)]}
        manager <- newManager tlsManagerSettings
        resp <- httpLbs req0 manager
        if statusCode (responseStatus resp) /= 200
          then do
            let body :: String = LChar8.unpack $ responseBody resp
            error $
              [i|Failed to download input for year #{year} day #{day} => #{body}|]
          else do
            let body :: String = LChar8.unpack $ responseBody resp
            writeFile (local_path <> local_file) body
            pure ()
  -- Ensure the directory exists
  void $ do
      exists <- doesDirectoryExist local_path
      if not exists then createDirectory local_path else pure ()
  -- Does the file exist?
  void $ do
      exists <- doesFileExist (local_path <> local_file)
      if not exists then downloadFile else pure ()
  openFile (local_path <> local_file) ReadMode >>= hGetContents
