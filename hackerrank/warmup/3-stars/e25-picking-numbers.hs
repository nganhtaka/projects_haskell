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
-- Complete the 'pickingNumbers' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY a as parameter.


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    let result = Data.List.maximum $ Data.List.map (Data.List.length) $ Data.List.groupBy (\a b -> abs (a-b) <= 1) $ Data.List.sort a

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
