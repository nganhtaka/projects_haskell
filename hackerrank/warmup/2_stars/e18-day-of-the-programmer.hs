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
import Data.Time
import Data.Time.Calendar.Julian

--
-- Complete the 'dayOfProgrammer' function below.
--
-- The function is expected to return a STRING.
-- The function accepts INTEGER year as parameter.
--

reFormatDate s = (Data.List.take 2 $ Data.List.drop 8 s) ++ "."
    ++(Data.List.take 2 $ Data.List.drop 5 s) ++ "."
    ++(Data.List.take 4 s)

dayOfProgrammer year  
    | year < 1918 = showJulian $ addDays 255 $ fromJulian year 1 1
    | year == 1918 = showGregorian $ addDays (255+13) (fromGregorian year 1 1)
    | otherwise = showGregorian $ addDays 255 (fromGregorian year 1 1)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    yearTemp <- getLine
    let year = read $ lstrip $ rstrip yearTemp :: Integer

    let result = reFormatDate $ dayOfProgrammer year

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
