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
-- Complete the 'formingMagicSquare' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts 2D_INTEGER_ARRAY s as parameter.
--
-- get max 8
--getMax 0 s res = res

getAllSum _ (-1) _ res = res
getAllSum i j (x:xs) res = 
    let sumHorizontal = (Data.List.take 3 res)
        sumVertical = (Data.List.take 3 (Data.List.drop 3 res))
        [d1,d2] = (Data.List.take 2 (Data.List.drop 6 res))
        newRes = (Data.List.take i sumHorizontal) ++ [(sum x)] ++ (Data.List.drop (i+1) sumHorizontal) 
                ++ (Data.List.zipWith (+) x sumVertical)
                ++ [(d1 + (Data.List.head (Data.List.drop i x))), (d2 + (Data.List.head (Data.List.drop j x)))]
    in do getAllSum (i+1) (j-1) xs newRes


formingMagicSquare s = 1
    -- Write your code here

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    sTemp <- readMultipleLinesAsStringArray 3
    let s = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) sTemp

    let result = formingMagicSquare s

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
