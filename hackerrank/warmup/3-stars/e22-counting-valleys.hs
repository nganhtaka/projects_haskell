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
-- Complete the 'countingValleys' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER steps
--  2. STRING path
--
countingValleys :: String -> Int -> Int
countingValleys [] _ = 0
countingValleys (p:path) hauteur
    | p == 'U' = countingValleys path (hauteur+1)
    | otherwise = if hauteur == 0 then 1 + (countingValleys path (hauteur-1)) else countingValleys path (hauteur-1)


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    stepsTemp <- getLine
    let steps = read $ lstrip $ rstrip stepsTemp :: Int

    path <- getLine

    let result = countingValleys path 0

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
