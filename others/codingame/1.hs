import System.IO
import Control.Monad
import Data.Char(digitToInt)


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem stateme== "|"nt.
    
    input_line <- getLine
    let speed1 = ((words input_line) !! 0) !! 1

    input_line <- getLine
    let secondLine = (words input_line) !! 0
    let speed =  digitToInt (secondLine !! 0)
    let speed2 = secondLine !! 1

    let testFirstLine = speed1 == '|' || ((digitToInt speed1) :: Int) >= speed
    let testSndLine = (speed2 == '|') || ((digitToInt speed2) :: Int) /= speed

    input_line <- getLine
    let speed3 = ((words input_line) !! 0) !! 1

    let testLastLine = speed3 == '|' || ((digitToInt speed3) :: Int) <= speed
    let result = if (testFirstLine &&  testSndLine && testLastLine) == True then "true" else "false"

    putStrLn $ result
    
    return ()