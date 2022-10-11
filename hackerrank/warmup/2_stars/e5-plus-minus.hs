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
-- Complete the 'plusMinus' function below.
--
-- The function accepts INTEGER_ARRAY arr as parameter.
--
    
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Float

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp
    let plus = Data.List.sum $ Data.List.map (\a -> 1) $ Data.List.filter (>0) arr
    let minus = Data.List.sum $ Data.List.map (\a -> 1) $ Data.List.filter (<0) arr
    let plusMinus = (show (plus/n)) ++ "\n" ++ (show (minus/n)) ++ "\n" ++ (show ((n-plus-minus)/n))

    putStrLn $ plusMinus
