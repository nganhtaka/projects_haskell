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
-- Complete the 'countApplesAndOranges' function below.
--
-- The function accepts following parameters:
--  1. INTEGER s
--  2. INTEGER t
--  3. INTEGER a
--  4. INTEGER b
--  5. INTEGER_ARRAY apples
--  6. INTEGER_ARRAY oranges
--

countFruits _ _ _ [] x = x
countFruits s t p (x:fruits) res = let po = (x+p) in do 
    if (s <= po) && (po <= t) then countFruits s t p fruits (res+1) else countFruits s t p fruits res

countApplesAndOranges s t a b apples oranges = [countFruits s t a apples 0, countFruits s t b oranges 0]
    -- Write your code here

getResult [x,y] = (show x) ++ "\n" ++ (show y)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    firstMultipleInputTemp <- getLine
    let firstMultipleInput = Data.List.words $ rstrip firstMultipleInputTemp

    let s = read (firstMultipleInput !! 0) :: Int

    let t = read (firstMultipleInput !! 1) :: Int

    secondMultipleInputTemp <- getLine
    let secondMultipleInput = Data.List.words $ rstrip secondMultipleInputTemp

    let a = read (secondMultipleInput !! 0) :: Int

    let b = read (secondMultipleInput !! 1) :: Int

    thirdMultipleInputTemp <- getLine
    let thirdMultipleInput = Data.List.words $ rstrip thirdMultipleInputTemp

    let m = read (thirdMultipleInput !! 0) :: Int

    let n = read (thirdMultipleInput !! 1) :: Int

    applesTemp <- getLine

    let apples = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip applesTemp

    orangesTemp <- getLine

    let oranges = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip orangesTemp

    putStrLn $ getResult (countApplesAndOranges s t a b apples oranges)
