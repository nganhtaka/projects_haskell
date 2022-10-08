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
-- Complete the 'staircase' function below.
--
-- The function accepts INTEGER n as parameter.
--

staircase 0 n result = result ++ (Data.List.replicate (n) '#')
staircase n i result = staircase (n-1) (i+1) (result ++ (Data.List.replicate (n) ' ') ++ (Data.List.replicate (i) '#') ++ "\n")

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    putStrLn $ (staircase (n-1) 1 "")
