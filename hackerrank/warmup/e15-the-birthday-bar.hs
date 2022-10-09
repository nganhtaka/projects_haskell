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
-- Complete the 'birthday' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER_ARRAY s
--  2. INTEGER d
--  3. INTEGER m
--

birthday _ (-1) _ _ res = res
birthday s i d m res 
    | Data.List.sum (Data.List.take m (Data.List.drop i s)) == d = birthday s (i-1) d m (res+1)
    | otherwise = birthday s (i-1) d m res

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    sTemp <- getLine

    let s = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip sTemp

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let d = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    let result = birthday s (n-m) d m 0

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
