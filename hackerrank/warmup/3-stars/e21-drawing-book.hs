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
-- Complete the 'pageCount' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER n
--  2. INTEGER p
--
pageCount :: Int -> Int -> Int
pageCount n p = let aPage = (div p 2) in do Data.List.minimum [aPage, (div n 2)-aPage]

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    pTemp <- getLine
    let p = read $ lstrip $ rstrip pTemp :: Int

    let result = pageCount n p

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
