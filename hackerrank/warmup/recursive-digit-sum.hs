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
-- Complete the 'superDigit' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. STRING n
--  2. INTEGER k
--

numToList :: String -> [Int]
numToList n = Data.List.map (\x -> read [x]::Int) n

superDigit :: String -> String
superDigit s = if Data.List.length s == 1 then s else
        superDigit (show (sum (numToList s)))


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = firstMultipleInput !! 0

    let k = read (firstMultipleInput !! 1) :: Int

    let result = superDigit (show (k * sum (numToList n)))

    hPutStrLn fptr $ result

    hFlush fptr
    hClose fptr
