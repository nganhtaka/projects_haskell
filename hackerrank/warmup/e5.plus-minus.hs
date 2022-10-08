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
-- Complete the 'plusMinus' function below.
--
-- The function accepts INTEGER_ARRAY arr as parameter.
--
plusMinus :: [Int] -> [Float] -> [Float]
plusMinus [] x = x
plusMinus (a:arr) [plus,minus] = 
    if a > 0 then plusMinus arr [(plus+1),minus] else
        if a < 0 then plusMinus arr [plus,(minus+1)]
            else plusMinus arr [plus,minus]

getResult :: Float -> [Float] -> String
getResult n [plus,minus] = 
    (show (plus/n)) ++ "\n" ++ (show (minus/n)) ++ "\n" 
    ++ (show ((n-plus-minus)/n))
    
lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    nTemp <- getLine
    let n = read $ lstrip $ rstrip nTemp :: Float

    arrTemp <- getLine

    let arr = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip arrTemp

    putStrLn $ getResult n (plusMinus arr [0,0])
    
