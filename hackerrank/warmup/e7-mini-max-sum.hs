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
-- Complete the 'miniMaxSum' function below.
--
-- The function accepts INTEGER_ARRAY arr as parameter.
--

miniMaxSum :: Int -> [Int] -> [Int] -> [Int]
miniMaxSum 0 _ x = x
miniMaxSum n arr [minS, maxS] = 
    let newS = sum ((Data.List.take (n-1) arr)  ++ (Data.List.drop n arr))
        newMin = if newS < minS then newS else minS  
        newMax = if newS > maxS then newS else maxS
    in do miniMaxSum (n-1) arr [newMin, newMax]

getResult [x,y] = (show x) ++ " " ++ (show y)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    putStrLn $ getResult (miniMaxSum 5 arr [(sum arr), 0])


    
