module Main (main) where

import System.IO  
import Control.Monad
import Day01 (solve)
filename = "day01-part1"
--filename = "test"

main :: IO ()
main = do  
        let list = []
        handle <- openFile ("inputs/" ++ filename) ReadMode
        contents <- hGetContents handle
        let result = solve contents
        print result
        hClose handle   


