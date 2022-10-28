{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Data.Array
import Data.List
import Data.Text
import Debug.Trace
import System.Environment
import System.IO

--
-- Complete the 'circularPalindromes' function below.
--
-- The function is expected to return an INTEGER_ARRAY.
-- The function accepts STRING s as parameter.
--
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = if (x == Data.List.last xs) then isPalindrome $ Data.List.init xs else False

countPalindrom [] = 0
countPalindrom (x:xs) = if isPalindrome x then Data.List.length x else countPalindrom xs

getAllList [] = 0
getAllList s = Data.List.maximum [(countPalindrom $ Data.List.tails $ Data.List.reverse s),  (getAllList $ Data.List.tail s)]

getAllCircular _ 0 _ = ""
getAllCircular i n xs = let aList = Data.List.drop i xs ++ Data.List.take i xs in do
    (show $ getAllList aList) ++ "\n" ++ (getAllCircular (i+1) (n-1) xs)

circularPalindromes s = getAllCircular 0 (Data.List.length s) s

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Int

    s <- getLine

    let result = circularPalindromes s

    hPutStrLn fptr $ result

    hFlush fptr
    hClose fptr


-- Tests :
-- circularPalindromes "cacbbba"
-- circularPalindromes "eededdeedede"
-- circularPalindromes "aaaaabbbbaaaa"