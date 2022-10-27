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
-- Complete the 'sherlockAndAnagrams' function below.
--
-- The function is expected to return an INTEGER.
-- The function accepts STRING s as parameter.
--
sortGT a1 a2
  | Data.List.length a1 < Data.List.length a2 = LT
  | Data.List.length a1 > Data.List.length a2 = GT
  | Data.List.length a1 == Data.List.length a2 = compare a1 a2

countOccurence tmp [x] = tmp*2
countOccurence tmp (x:y:xs)
    | x /= y = (sum [1..tmp]) + (countOccurence 0 (y:xs))
    | otherwise = countOccurence (tmp+1) (y:xs)

getAllList [] = []
getAllList s = (Data.List.tail $ Data.List.map Data.List.sort $ Data.List.inits s) ++ (getAllList $ Data.List.tail s)

sherlockAndAnagrams s = countOccurence 0 $ Data.List.sortBy sortGT $ getAllList s

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    qTemp <- getLine
    let q = read $ lstrip $ rstrip qTemp :: Int

    forM_ [1..q] $ \q_itr -> do
        s <- getLine

        let result = sherlockAndAnagrams s

        hPutStrLn fptr $ show result

    hFlush fptr
    hClose fptr
