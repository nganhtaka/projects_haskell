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
import Data.Maybe

--
-- Complete the 'migratoryBirds' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts INTEGER_ARRAY arr as parameter.
--
migratoryBirds :: [Int] -> [Int] -> [Int]
migratoryBirds [] res = res
migratoryBirds (a:arr) res = let i = a-1 in do
    migratoryBirds arr (Data.List.take i res ++ [(Data.List.head (Data.List.drop i res)) + 1] ++  Data.List.drop (i+1) res)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    arrCountTemp <- getLine
    let arrCount = read $ lstrip $ rstrip arrCountTemp :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    let aList = migratoryBirds arr (Data.List.replicate arrCount 0)
    let result = case (Data.List.elemIndex (Data.List.maximum aList) aList) of
             Just n -> n+1
             Nothing -> 0

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
