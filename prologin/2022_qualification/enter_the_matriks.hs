-- https://prologin.org/train/2022/qualification/enter-the-matriks
canGetSum :: Int -> [Int] -> Int -> [Int]
canGetSum 0 _ _ = []
canGetSum _ [] _ = []
canGetSum goal [x] _ = if goal == x then [x] else []
canGetSum goal xy 0 = canGetSum goal (tail xy) ((length xy)-1)
canGetSum goal xy n = if (goal == (sum (take n xy))) then (take n xy) else (canGetSum goal xy (n-1))

calculMagic :: Int -> [Int] -> [Int] -> Int -> Int -> Int -> [[Int]]
calculMagic 0 _ _ _ _ _ = [[]]
calculMagic _ [] _ _ _ _ = [[]]
calculMagic _ _ [] _ _ _ = [[]]
calculMagic goal xs xy _ 0 t = calculMagic goal xs (tail xy) 0 ((length xy)-1) t
calculMagic goal xs xy i n t = if (mod goal (sum (drop i (take n xy)))) == 0 then 
  [(drop i (take n xy)), (canGetSum (div goal (sum (drop i (take n xy)))) xs (length xs))] else 
    (if ((i+n) < t) then (calculMagic goal xs xy (i+1) n t) else (calculMagic goal xs xy 0 (n-1) t))

listIntToString :: [Int] -> String
listIntToString [x] = show x
listIntToString (x:xs) = (listIntToString [x]) ++ " " ++ (listIntToString xs)

magicToString :: [[Int]] -> String
magicToString [[],_] = "IMPOSSIBLE"
magicToString [_,[]] = "IMPOSSIBLE"
magicToString [x, y] = if (length x > length y) || (sum x) >= (sum y) then (listIntToString x) ++ "\n" ++ (listIntToString y)
  else (listIntToString y) ++ "\n" ++ (listIntToString x)

resoudre :: Int     -- ^ le nombre magique
         -> Int     -- ^ la longueur du code la Matriks
         -> [Int]   -- ^ le code de la Matriks
         -> String  -- ^ TODO
-- Les deux clés (chacune sur une ligne) ou le message "IMPOSSIBLE".
resoudre 0 _ _ = "IMPOSSIBLE"
resoudre _ 0 _ = "IMPOSSIBLE"
resoudre _ _ [] = "IMPOSSIBLE"
resoudre x n l = magicToString (calculMagic x l l 0 n n)

main :: IO ()
main = do
  x <- fmap read getLine
  n <- fmap read getLine
  l <- fmap (map read . words) getLine
  putStrLn $ resoudre x n l