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
-- Complete the 'getTotalX' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts following parameters:
--  1. INTEGER_ARRAY a
--  2. INTEGER_ARRAY b
getLeft :: [Int] -> Int -> Int
getLeft [] x = x
getLeft ys x = 
    let y = Data.List.last ys in do
    if mod x y == 0 then (getLeft (Data.List.init ys) x) else ((div x y)+1)*y

getRight :: [Int] -> Int -> Int
getRight [] y = y
getRight (x:xs) y = 
    if y <= x && mod x y == 0 then getRight xs y else div x ((div x y)+1)

getTotalX :: [Int] -> [Int] -> Int -> Int -> [Int] -> [Int]
getTotalX a b x y res =
    if x > y then res else 
        let newLeft = getLeft a x
            newRight = getRight b x
        in do if (newLeft == newRight) then getTotalX a b (newLeft+1) y res++[newLeft] else
                if (newLeft == x) then getTotalX a b (newLeft+1) y res
                else getTotalX a b newLeft y res

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let n = read (firstMultipleInput !! 0) :: Int

    let m = read (firstMultipleInput !! 1) :: Int

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    brrTemp <- getLine

    let brr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip brrTemp

    let total = Data.List.length (getTotalX arr brr (Data.List.maximum arr) (Data.List.maximum brr) [])

    hPutStrLn fptr $ show total

    hFlush fptr
    hClose fptr
