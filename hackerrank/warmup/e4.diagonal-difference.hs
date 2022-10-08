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
-- Complete the 'diagonalDifference' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts 2D_INTEGER_ARRAY arr as parameter.
--

diagonalDifference 0 _ _ _ [first,last] = [first,last]
diagonalDifference n (liste:arr) l r [first,last] = 
    let left = (Data.List.head (Data.List.drop l liste))
        right = (Data.List.head (Data.List.drop r liste))
    in do diagonalDifference (n-1) arr (l+1) (r-1) [(first+left),(last+right)]

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

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    arrTemp <- readMultipleLinesAsStringArray n
    let arr = Data.List.map (\x -> Data.List.map (read :: String -> Int) . Data.List.words $ rstrip x) arrTemp

    let [first,last] = diagonalDifference n arr 0 (n-1) [0,0]
    let result = abs (first - last)

    hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
