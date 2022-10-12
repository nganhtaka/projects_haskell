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

squaresGenerated = Data.List.filter (\x -> getAllSum x == [15,15,15,15,15,15,15,15]) $ Data.List.map listToSquare $ Data.List.permutations [1,2,3,4,5,6,7,8,9]

listToSquare x = [Data.List.take 3 x, Data.List.take 3 $ Data.List.drop 3 x, Data.List.take 3 $ Data.List.drop 6 x]

getSumHorizontal x = [Data.List.sum $ x !! 0, Data.List.sum $ x !! 1, Data.List.sum $ x !! 2]
getSumVertical x = getSumHorizontal $ Data.List.transpose x

getSumDiagonalLeft x = Data.List.sum $ Data.List.zipWith (\x y -> x !! y) x [0,1,2]
getSumDiagonalRight x = Data.List.sum $ Data.List.zipWith (\x y -> x !! y) x [2,1,0]

getAllSum x = getSumHorizontal x ++ getSumVertical x ++ [getSumDiagonalLeft x, getSumDiagonalRight x]

-- [[5,3,4],[1,5,8],[6,4,2]] -- [[6,1,8],[7,5,3],[2,9,4]]

compareToGetPoint [] [] = 0
compareToGetPoint (s1:square1) (s2:square2) =
    (Data.List.sum $ Data.List.zipWith (\x y -> abs (x-y)) s1 s2) + (compareToGetPoint square1 square2)

formingMagicSquare s = Data.List.minimum $ Data.List.map (compareToGetPoint s) $ squaresGenerated 

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
