
import Control.Monad (replicateM)

-- | Fil reliant deux puces
data Fil = Fil
  { puce1 :: Int  -- ^ première extrémité du fil
  , puce2 :: Int  -- ^ seconde extrémité du fil
  }

-- | Question posée par Joseph
data Question = Question
  { puceA :: Int  -- ^ première extrémité alimentée
  , puceB :: Int  -- ^ seconde extrémité alimentée
  }

data Puce = Puce { name::Int, signal:: Int}

data Node = Node { parent :: Puce, child :: [Int]}

haveTheSameElement :: [Int] -> [Int] -> [Int] -> [Int]
haveTheSameElement [] _ result = result
haveTheSameElement (x:xs) ys result = if (elem x ys) then haveTheSameElement xs ys (result ++ [x]) else haveTheSameElement xs ys result

chercheAChemin :: Question -> [Node] -> [Int] -> Int -> Int
chercheAChemin (Question puceA puceB) tree chemin result = 
  let (Node (Puce puce1 signal1) listPuces1) = head (drop puceA tree)
      (Node (Puce puce2 signal2) listPuces2) = head (drop puceB tree)
  in do 
    if signal1 == 0 || signal2 == 0 then 0 else
      if elem puce1 chemin then 1671404011 else
        if elem puce1 listPuces2 then (mod (signal1*signal2*result) 1671404011)
            else let listCommun = (haveTheSameElement listPuces1 listPuces2 []) in do
              if length listCommun > 0 then 
                let (Node (Puce _ signal11) _) = head (drop (head listCommun) tree)
                in do (mod (signal1 * signal11 * signal2 * result) 1671404011)
              else minimum (map (\x -> chercheAChemin (Question x puceB) tree (chemin++[puce1]) (mod (result*signal1) 1671404011)) listPuces1)


createTree :: [Fil] -> [Node] -> [Node]
createTree [] tree = tree
createTree ((Fil puce1 puce2):xs) tree = 
  let (Node aPuce1 listePuce1) = head (drop puce1 tree)
      (Node aPuce2 listePuce2) = head (drop puce2 tree)
      treeWithPuce1 = ((take puce1 tree) ++ [(Node aPuce1 (listePuce1 ++ [puce2]))] ++ (drop (puce1+1) tree))
      treeWithPuce2 = ((take puce2 treeWithPuce1) ++ [(Node aPuce2 (listePuce2 ++ [puce1]))] ++ (drop (puce2+1) treeWithPuce1))
  in do createTree xs treeWithPuce2

createEmptyTree :: Int -> [Int] -> [Node] -> [Node]
createEmptyTree _ [] tree = tree
createEmptyTree i (x:xs) tree = createEmptyTree (i+1) xs (tree ++ [(Node (Puce i x) [])])

showListInt :: [Int] -> String
showListInt [] = ""
showListInt [x] = show x
showListInt (x:xs) = show x ++ "\n" ++ showListInt xs

showPuce :: Puce -> String
showPuce (Puce x y) = "(" ++ show x ++ "," ++ show y ++ ")"

showNode :: Node -> String
showNode (Node x ys) = "(" ++ (showPuce x) ++ "[" ++ (showListInt ys) ++ "])"

calculerSignaux :: Int         -- ^ nombre de puces
                -> Int         -- ^ nombre de fils
                -> Int         -- ^ nombre de questions
                -> [Int]       -- ^ liste des signaux
                -> [Fil]       -- ^ liste des fils entre les puces
                -> [Question]  -- ^ liste des questions
                -> String      -- ^ TODO
-- Affiche le signal envoyé au coffre-fort pour chaque requête
calculerSignaux n m r signaux fils questions 
  | n < 1 || n > 100000 || (m /= (n-1)) || r < 1 || r > 100000 = ""
  | n /= length signaux || m /= length fils || r /= length questions = ""
  | otherwise = let tree = (createTree fils (createEmptyTree 0 signaux [])) 
                in do showListInt (map (\x -> chercheAChemin x tree [] 1) questions)

main :: IO ()
main = do
  n <- fmap read getLine
  m <- fmap read getLine
  r <- fmap read getLine
  signaux <- fmap (map read . words) getLine
  fils <- replicateM m readFil
  questions <- replicateM r readQuestion
  putStrLn $ calculerSignaux n m r signaux fils questions
  where
    readFil = fmap ((\[a, b] -> Fil (read a) (read b)) . words) getLine
    readQuestion = fmap ((\[a, b] -> Question (read a) (read b)) . words) getLine
