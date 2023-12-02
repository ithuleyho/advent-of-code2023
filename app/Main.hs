module Main (main) where

import System.IO  
import Data.Char

import Day02 (solve)

filename :: String
filename = "day02-part1"
-- filename = "test"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main :: IO ()
main = do  
        handle <- openFile ("inputs/" ++ filename) ReadMode
        contents <- hGetContents handle
        let result = solve $ trim contents
        print result
        hClose handle   


