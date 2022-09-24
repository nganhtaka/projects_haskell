-- https://prologin.org/train/2022/qualification/derniere_piece
import Control.Monad (replicateM)

-- | Structure pour designer une piece
data Structpiece = Structpiece
  { ncotespiece  :: Int     -- ^ le nombre de cotes de la pièce
  , couleurpiece :: String  -- ^ la couleur de la pièce
  }

compareCouleur :: [String] -> String -> Structpiece
compareCouleur [x] y = if x == y then (Structpiece 0 "X") else (Structpiece 1 "O")
compareCouleur (x:xs) y = if x == y then (Structpiece 0 "X") else (compareCouleur xs y)

checkCote :: Int -> [String] -> Structpiece -> Structpiece
checkCote ncotes1 couleurscotes1 (Structpiece ncotes2 couleurscotes2) = 
    (if ncotes1 /= ncotes2 then (Structpiece 0 "X") else (compareCouleur couleurscotes1 couleurscotes2))

combineStructpiece :: Structpiece -> Structpiece -> Structpiece
combineStructpiece (Structpiece a b) (Structpiece x y) = (Structpiece (a+x) (b++y))

structpieceToString :: Structpiece -> String
structpieceToString (Structpiece a b) = b ++ "\n" ++ (show a)

resoudreIntermediaire :: Int            -- ^ le nombre de couleurs
         -> [String]       -- ^ les différentes couleurs possibles
         -> Int            -- ^ le nombre de côtés de la pièce manquante
         -> [String]       -- ^ les couleurs des pièces adjacentes à la pièce manquante
         -> Int            -- ^ le nombre de pièces à trier
         -> [Structpiece]  -- ^ les pièces à trier
         -> Structpiece         -- ^ TODO
resoudreIntermediaire _ _ ncotes couleurscotes 1 [x] = checkCote ncotes couleurscotes x
resoudreIntermediaire ncouleurs couleurs ncotes couleurscotes npieces (x:xs) = 
    (combineStructpiece (checkCote ncotes couleurscotes x)
    (resoudreIntermediaire ncouleurs couleurs ncotes couleurscotes (npieces-1) xs))

resoudre :: Int            -- ^ le nombre de couleurs
         -> [String]       -- ^ les différentes couleurs possibles
         -> Int            -- ^ le nombre de côtés de la pièce manquante
         -> [String]       -- ^ les couleurs des pièces adjacentes à la pièce manquante
         -> Int            -- ^ le nombre de pièces à trier
         -> [Structpiece]  -- ^ les pièces à trier
         -> String         -- ^ TODO
-- Affiche sur la première ligne, pour chaque pièce un caractère 'O' si la
-- pièce peut correspondre à celle recherchée, 'X' sinon. Affiche sur la ligne
-- suivante le nombre de pièces qui peuvent correspondre.
resoudre ncouleurs couleurs ncotes couleurscotes npieces pieces = structpieceToString
    (resoudreIntermediaire ncouleurs couleurs ncotes couleurscotes npieces pieces)

main :: IO ()
main = do
  ncouleurs <- fmap read getLine
  couleurs <- replicateM ncouleurs getLine
  ncotes <- fmap read getLine
  couleurscotes <- replicateM ncotes getLine
  npieces <- fmap read getLine
  pieces <- replicateM npieces readStructpiece
  putStrLn $ resoudre ncouleurs couleurs ncotes couleurscotes npieces pieces
  where
    readStructpiece = Structpiece <$> fmap read getLine <*> getLine
