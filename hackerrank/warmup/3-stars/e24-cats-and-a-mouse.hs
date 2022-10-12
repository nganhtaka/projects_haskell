{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the catAndMouse function below.
catAndMouse x y z = 
    let cat1 = abs (x - z)
        cat2 = abs (y - z)
    in do if cat1 > cat2 then "Cat B" else if cat1 < cat2 then "Cat A" else "Mouse C"

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    q <- readLn :: IO Int

    forM_ [1..q] $ \q_itr -> do
        xyzTemp <- getLine
        let xyz = words xyzTemp

        let x = read (xyz !! 0) :: Int

        let y = read (xyz !! 1) :: Int

        let z = read (xyz !! 2) :: Int

        let result = catAndMouse x y z

        hPutStrLn fptr result

    hFlush fptr
    hClose fptr
