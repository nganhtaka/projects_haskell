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
-- Complete the 'compareTriplets' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts following parameters:
--  1. INTEGER_ARRAY a
--  2. INTEGER_ARRAY b
--

compareTriplets [] _ [pA,pB] = [pA,pB]
compareTriplets (a:as) (b:bs) [pA,pB] = 
    if (a > b) then compareTriplets as bs [(pA+1),pB] else
        if (a < b) then compareTriplets as bs [pA,(pB+1)] else
            compareTriplets as bs [pA,pB]
            
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp

    let result = compareTriplets a b [0,0]

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
