{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Data.Text
import Data.Char
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'camelcase' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts STRING s as parameter.
--

camelcase [] = 1
camelcase (x:xs) = if (Data.Char.isUpper x) then (1 + (camelcase xs)) else camelcase xs


main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = camelcase s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
