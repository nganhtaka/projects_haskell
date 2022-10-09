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
-- Complete the 'kangaroo' function below.
--
-- The function is expected to return a STRING.
-- The function accepts following parameters:
--  1. INTEGER x1
--  2. INTEGER v1
--  3. INTEGER x2
--  4. INTEGER v2
--
kangaroo x1 v1 x2 v2 = let s1 = (x1 - x2)
                           s2 = (v2 - v1) in do
                            if s1 == 0 && s2 == 0 then "YES" else
                                if s1 /= 0 && s2 /= 0 && mod s1 s2 == 0 && (div s1 s2) > 0 then "YES" else "NO"


lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let x1 = read (firstMultipleInput !! 0) :: Int

    let v1 = read (firstMultipleInput !! 1) :: Int

    let x2 = read (firstMultipleInput !! 2) :: Int

    let v2 = read (firstMultipleInput !! 3) :: Int

    let result = kangaroo x1 v1 x2 v2

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
