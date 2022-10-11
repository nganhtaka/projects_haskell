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
-- Complete the 'bonAppetit' function below.
--
-- The function accepts following parameters:
--  1. INTEGER_ARRAY bill
--  2. INTEGER k
--  3. INTEGER b
--

bonAppetit bill k b = let forA = div (Data.List.sum (Data.List.take k bill++ Data.List.drop (k+1) bill)) 2
    in do if forA == b then "Bon Appetit" else show (b-forA)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let k = read (firstMultipleInput !! 1) :: Int

    billTemp <- getLine

    let bill = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip billTemp

    bTemp <- getLine
    let b = read $ lstrip $ rstrip bTemp :: Int

    putStrLn $ bonAppetit bill k b
