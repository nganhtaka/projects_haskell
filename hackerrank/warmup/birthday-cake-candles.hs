{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'birthdayCakeCandles' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY candles as parameter.
--

birthdayCakeCandles :: Int -> [Int] -> [Int] -> [Int]
birthdayCakeCandles 0 _ x = x
birthdayCakeCandles n (ca:candles) [nb,maxC] =
    if (ca > maxC) then birthdayCakeCandles (n-1) candles [1,ca] else
        if (ca == maxC) then birthdayCakeCandles (n-1) candles [(nb+1),maxC] else
            birthdayCakeCandles (n-1) candles [nb,maxC]


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    candlesCountTemp <- getLine
    let candlesCount = read $ lstrip $ rstrip candlesCountTemp :: Int

    candlesTemp <- getLine

    let candles = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip candlesTemp

    let result = Data.List.head (birthdayCakeCandles (read candlesCountTemp :: Int) candles [0,0])
    
    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
