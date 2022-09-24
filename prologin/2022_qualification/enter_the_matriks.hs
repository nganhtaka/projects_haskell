-- https://prologin.org/train/2022/qualification/enter-the-matriks
canGetSum :: Int -> [Int] -> Int -> [Int]
canGetSum _ [] _ = []
canGetSum goal xy n = if (goal == (sum (take n xy))) then (take n xy) else
  (if n == 0 then canGetSum goal (tail xy) ((length xy)-1) else (canGetSum goal xy (n-1)))

calculMagic :: Int -> [Int] -> [Int] -> Int -> [[Int]]
calculMagic _ [] _ _ = [[]]
calculMagic _ _ [] _ = [[]]
calculMagic goal xs xy n = if (mod goal (sum (take n xy))) == 0 then 
  [(take n xy), (canGetSum (div goal (sum (take n xy))) xs n)] else 
    (if (n == 0) then (calculMagic goal xs (tail xy) ((length xy)-1)) else (calculMagic goal xs xy (n-1)))

listIntToString :: [Int] -> String
listIntToString [x] = show x
listIntToString (x:xs) = (listIntToString [x]) ++ " " ++ (listIntToString xs)

magicToString :: [[Int]] -> String
magicToString [[],_] = "IMPOSSIBLE"
magicToString [_,[]] = "IMPOSSIBLE"
magicToString [x, y] = if (sum x) >= (sum y) then (listIntToString x) ++ "\n" ++ (listIntToString y)
  else (listIntToString y) ++ "\n" ++ (listIntToString x)

resoudre :: Int     -- ^ le nombre magique
         -> Int     -- ^ la longueur du code la Matriks
         -> [Int]   -- ^ le code de la Matriks
         -> String  -- ^ TODO
-- Les deux clés (chacune sur une ligne) ou le message "IMPOSSIBLE".
resoudre x n l = magicToString (calculMagic x l l n)

main :: IO ()
main = do
  x <- fmap read getLine
  n <- fmap read getLine
  l <- fmap (map read . words) getLine
  putStrLn $ resoudre x n l