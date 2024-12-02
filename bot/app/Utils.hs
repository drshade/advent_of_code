module Utils where

stripNewlines :: String -> String
stripNewlines = filter (/= '\n')
