-- https://prologin.org/train/2022/qualification/enter-the-matriks
data Structmatrik = Structmatrik { firstListe  :: [Int], secondListe :: [Int]}

canGetSum :: Int -> [Int] -> Int -> Int -> Int ->  [[Int]] -> Structmatrik -> Structmatrik
canGetSum _ _ _ _ _ [] result = result
canGetSum goal liste i 0 nbTotal (x:xs) result = canGetSum goal liste 0 nbTotal nbTotal xs result 
canGetSum goal liste (-1) step nbTotal xs result = canGetSum goal liste (nbTotal-step+1) (step-1) nbTotal xs result 
canGetSum goal liste i step nbTotal (x:xs) result = 
  let resteGoal = div goal (sum x)
      secondListe = take step (drop i liste)
      var = (Structmatrik x secondListe)
  in do
    if (resteGoal == (sum secondListe)) && (countElementIn2List var result)
    then (canGetSum goal liste i step nbTotal xs var)
    else (canGetSum goal liste (i-1) step nbTotal (x:xs) result)

calculMagic :: Int -> [Int]-> Int -> Int -> Int -> [[Int]] -> [[Int]]
calculMagic _ _ _ 0 _ result = result
calculMagic goal liste (-1) step nbTotal result = calculMagic goal liste (nbTotal-step+1) (step-1) nbTotal result
calculMagic goal liste i step nbTotal result = let firstListe = (take step (drop i liste)) in do 
  if (mod goal (sum firstListe)) == 0
  then calculMagic goal liste (i-1) step nbTotal ([firstListe] ++ result)
  else calculMagic goal liste (i-1) step nbTotal result

listIntToString :: [Int] -> String
listIntToString [x] = show x
listIntToString (x:xs) = (listIntToString [x]) ++ " " ++ (listIntToString xs)

magicToString :: Structmatrik -> String
magicToString (Structmatrik x y) 
  | x == [] || y == [] = "IMPOSSIBLE"
  | otherwise 
    | (length x > length y) || ((length x ==  length y) && (sum x) >= (sum y)) = (listIntToString x) ++ "\n" ++ (listIntToString y)
    | otherwise = (listIntToString y) ++ "\n" ++ (listIntToString x)

countElementIn2List :: Structmatrik -> Structmatrik -> Bool
countElementIn2List (Structmatrik x y) (Structmatrik a b)
    | (length x + length y) > (length a + length b) = True
    | otherwise = False

resoudre :: Int     -- ^ le nombre magique
         -> Int     -- ^ la longueur du code la Matriks
         -> [Int]   -- ^ le code de la Matriks
         -> String  -- ^ TODO
-- Les deux clés (chacune sur une ligne) ou le message "IMPOSSIBLE".
resoudre x n l 
  | x < 0 || n < 2 || n > 10000 || l == [] = "IMPOSSIBLE"
  | otherwise = magicToString (canGetSum x l 0 n n (calculMagic x l 0 n n []) (Structmatrik [] []))

main :: IO ()
main = do
  x <- fmap read getLine
  n <- fmap read getLine
  l <- fmap (map read . words) getLine
  putStrLn $ resoudre x n l