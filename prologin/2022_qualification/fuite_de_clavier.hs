import Data.Char (isUpper, isLower, isDigit, isPunctuation, isSymbol)

checkRules :: [Char] -> [Bool] -> Bool
checkRules _ [True, True, True, True] = True
checkRules [] bool = all (== True) bool
checkRules (x:xs) [a,b,c,d] = if all (== True) [a,b,c,d] then True else
    if a == False && (isUpper x) then checkRules xs [True,b,c,d] else
        if b == False && (isLower x) then checkRules xs [a,True,c,d] else
            if c == False && (isDigit x) then checkRules xs [a,b,True,d] else
                if d == False && ((isPunctuation x) || (isSymbol x)) then checkRules xs [a,b,c,True] else
                    checkRules xs [a,b,c,d]
    
getAllChaine :: [Char] -> Int -> Int -> Int -> Int
getAllChaine _ (-1) _ result = result
getAllChaine xs i pwSize result = let var = (take pwSize (drop i xs)) in do
    if (checkRules var [False, False, False, False]) then getAllChaine xs (i-1) pwSize (result+1)
    else getAllChaine xs (i-1) pwSize result

fuiteDeClavier :: Int     -- ^ taille de la chaîne
               -> Int     -- ^ taille du mot de passe
               -> [Char]  -- ^ la chaîne contenant le mot de passe
               -> String  -- ^ TODO
-- afficher le nombre de mots de passes possibles parmi la chaîne
fuiteDeClavier n k chaine
    | k < 4 = "0"
    | n < k = "0"
    | n > 1000000 = "0"
    | k > 10000 = "0"
    | otherwise = show (getAllChaine chaine (n-k) k 0)

main :: IO ()
main = do
  n <- fmap read getLine
  k <- fmap read getLine
  chaine <- getLine
  putStrLn $ fuiteDeClavier n k chaine