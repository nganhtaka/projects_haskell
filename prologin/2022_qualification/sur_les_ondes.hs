-- https://prologin.org/train/2022/qualification/sur_les_ondes
getNumber :: [Int] -> [Int]
getNumber (x:y:xs) = (if mod y 3 == 0 && y < x then (y:xs) else (x:xs))

surLesOndes :: Int     -- ^ nombre de fréquences données
            -> [Int]   -- ^ la liste des fréquences à vérifier
            -> String  -- ^ TODO
surLesOndes 0 _ = show 0
surLesOndes 1 [x] = show x
surLesOndes n (x:xs) = surLesOndes (n-1) (if mod x 3 == 0 then (getNumber (x:xs)) else xs)

main :: IO ()
main = do
  n <- fmap read getLine
  freqs <- fmap (map read . words) getLine
  putStrLn $ surLesOndes n freqs