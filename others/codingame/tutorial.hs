import System.IO
import Control.Monad

getMinTemp :: Int -> [Int] -> Int
getMinTemp 0 _ = 0
getMinTemp _ [x] = x
getMinTemp n (x:xs) = let res = getMinTemp n xs in do 
    if abs(x) < abs(res) then x else res

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let n = read input_line :: Int
    
    is_closed <- isEOF 
    if is_closed 
        then putStrLn "0" 
        else do input_line <- getLine
                let input = map (read :: String -> Int) $ words input_line
                -- Write answer to stdout
                putStrLn $ show $ getMinTemp n input
    return ()