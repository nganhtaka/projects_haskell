-- https://prologin.org/train/2022/qualification/enter-the-matriks
canGetSum :: Int -> [Int] -> Int -> Int -> Int -> [Int]
canGetSum 0 _ _ _ _ = []
canGetSum _ [] _ _ _ = []
canGetSum goal [x] _ _ _ = if goal == x then [x] else []
canGetSum goal liste _ 0 nbTotal = canGetSum goal (tail liste) 0 ((length liste)-1) nbTotal
canGetSum goal liste i step nbTotal = 
  if (goal == (sum (drop i (take step liste)))) 
  then (drop i (take step liste)) 
  else (if ((i+step) < nbTotal) 
        then (canGetSum goal liste (i+1) step nbTotal) 
        else (canGetSum goal liste 0 (step-1) nbTotal))

calculMagic :: Int -> [Int]-> Int -> Int -> Int -> [[Int]]
calculMagic 0 _ _ _ _  = [[]]
calculMagic _ [] _ _ _ = [[]]
calculMagic goal liste i step nbTotal = 
  if (mod goal (sum (drop i (take step liste)))) == 0 
  then [(drop i (take step liste)), (canGetSum (div goal (sum (drop i (take step liste)))) liste 0 nbTotal nbTotal)] 
  else (if ((i+step) < nbTotal) 
        then (calculMagic goal liste (i+1) step nbTotal) 
        else (calculMagic goal liste 0 (step-1) nbTotal))

listIntToString :: [Int] -> String
listIntToString [x] = show x
listIntToString (x:xs) = (listIntToString [x]) ++ " " ++ (listIntToString xs)

magicToString :: [[Int]] -> String
magicToString [[]] = "IMPOSSIBLE"
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
resoudre x n l = magicToString (calculMagic x l 0 n n)

main :: IO ()
main = do
  x <- fmap read getLine
  n <- fmap read getLine
  l <- fmap (map read . words) getLine
  putStrLn $ resoudre x n l