{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'breakingRecords' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts INTEGER_ARRAY scores as parameter.
--

breakingRecords [] _ res = res
breakingRecords (x:scores) [high, low] [scoreH, scoreL] 
    | x > high = breakingRecords scores [x, low] [scoreH+1, scoreL]
    | x < low = breakingRecords scores [high, x] [scoreH, scoreL+1] 
    | otherwise = breakingRecords scores [high, low] [scoreH, scoreL] 

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    scoresTemp <- getLine

    let scores = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip scoresTemp

    let result = breakingRecords scores [Data.List.head scores, Data.List.head scores] [0, 0]

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
