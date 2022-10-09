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
-- Complete the 'timeConversion' function below.
--
-- The function is expected to return a STRING.
-- The function accepts STRING s as parameter.
calculateTime :: String -> String
calculateTime s = (show ((read s ::Int) + 12))
        
timeConversion :: String -> String
timeConversion s = 
    let hour = Data.List.take 2 s
        minSecond = Data.List.take 6 (Data.List.drop 2 s) in do
    if (Data.List.drop 8 s) == "AM" then 
        if hour == "12" then ("00" ++ minSecond) else (hour ++ minSecond)
    else
        if hour == "12" then (hour ++ minSecond)
                else ((calculateTime hour) ++ minSecond)
    
main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    s <- getLine

    let result = timeConversion s

    hPutStrLn fptr result

    hFlush fptr
    hClose fptr
