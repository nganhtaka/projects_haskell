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
-- Complete the 'sockMerchant' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. INTEGER_ARRAY ar
--

sockMerchant [] = 0
sockMerchant [x] = 0
sockMerchant (x:y:ar) 
    | x == y = 1 + (sockMerchant ar)
    | x /= y = 0 + (sockMerchant (y:ar))

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arTemp <- getLine

    let ar = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arTemp

    let result = sockMerchant $ Data.List.sort ar

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
