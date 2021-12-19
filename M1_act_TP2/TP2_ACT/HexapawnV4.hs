-- -- -- -- -- -- -- --
--    Hexapawn v4    --
-- -- -- -- -- -- -- --
module Main where
import Data.Maybe (fromJust)
import Data.Map (lookup, empty, Map)
import qualified Data.Map as Map

-- B : Black, W : White
data Player    = Black | White deriving (Show, Eq, Ord)
data Piece     = B | W deriving (Show, Eq, Ord)
type Pos       = (Int, Int)
type Square    = (Pos, Maybe Piece)
type Board     = Map Pos (Maybe Piece)
data GameState = GameState Board Player deriving (Show, Ord)
instance Eq GameState where
    GameState b1 p1 == GameState b2 p2 = b1 == b2 && p1 == p2

main :: IO()
main = do
    contents <- getContents
    let (_n:_m:rest) = lines contents
        n            = read _n
        m            = read _m
        board        = readBoard n rest
        value        = calculConfig (GameState board White) n m
    putStrLn . show $ value

readBoard :: Int -> [[Char]] -> Board
readBoard n xs = readRows n Map.empty xs


readRows :: Int -> Map Pos (Maybe Piece) -> [[Char]] -> Map Pos (Maybe Piece)
readRows _ mapBoard [] = mapBoard
readRows ordonnee mapBoard (x:xs) =
    readRows (ordonnee-1) (readRow ordonnee 1 mapBoard x) xs


readRow :: Int -> Int -> Map Pos (Maybe Piece) -> [Char] -> Map Pos (Maybe Piece)
readRow _ _ mapBoard [] = mapBoard
readRow ordonnee abcisse mapBoard (x:xs) =
    if (x/='p' && x/='P') then readRow ordonnee (abcisse+1) mapBoard xs
    else readRow ordonnee (abcisse+1) (Map.insert (abcisse, ordonnee) (readPiece x) mapBoard) xs
    

readPiece :: Char -> Maybe Piece
readPiece 'p' = Just B
readPiece 'P' = Just W
readPiece ' ' = Nothing
readPiece _   = Nothing

getOpponent :: Player -> Player
getOpponent White = Black
getOpponent Black = White

getPieceType :: Player -> Piece
getPieceType White = W
getPieceType Black = B

getOpponentPiece :: Maybe Piece -> Maybe Piece
getOpponentPiece (Just W) = Just B
getOpponentPiece (Just B) = Just W
getOpponentPiece Nothing = Nothing

-- -- -- -- ---- -- --
--  **Question 1**  --
-- -- -- -- ---- -- --

{-  contientNegatif : Fonction verifier s'il y a des chiffres <= 0 dans la liste des sucesseur
    @param : une liste des entiers
    @return : True si la liste contient des valeurs inferieures ou egales a 0, False sinon
-}
contientNegatif :: [Int] -> Bool
contientNegatif listeValeurSuccesseur = (minimum listeValeurSuccesseur) <= 0



{-  calculValeur : Fonction calculer la configuration a partir d'une liste des configurations
    @param : une liste des entiers
    @return : valeur configure de cette liste. 
              Si tous les successeurs ont des valeurs positives : resultat = - (maximum des valeurs positif + 1)
              Sinon : resultat = -[(max valeurs<=0)-1]
-}
calculValeur :: [Int] -> Int
calculValeur [] = 0
calculValeur listeValeurSuccesseur = if (contientNegatif listeValeurSuccesseur)
                                        then -((maximum (filter (<=0) listeValeurSuccesseur))-1)
                                        else -((maximum listeValeurSuccesseur)+1)

-- -- -- -- ---- -- --
--  **Question 2**  --
-- -- -- -- ---- -- --

{-  moveY : Fonction verifier : si player White : quand il avance, son y incremente
                        si player Black : quand il avance, son y decremente
    @param : une piece, soit B soit W
    @return : une fonction, soit (+) incrementer, soit (-) decrementer
-}
moveY :: Piece -> (Int -> Int -> Int)
moveY W = (+)
moveY B = (-)


{-  calculConfig : fonction calculer les configuration d'un jeu donne
    @param : GameState : contient   - board : la board d'un jeu, avec les pions et sa position [(position, pion)]
                                    - player : soit White soit Black
             n : hauteur de la board
             m : largeur de la board
    @return : la configuration du jeu type Int
-}
calculConfig :: GameState -> Int -> Int -> Int
calculConfig (GameState board player) n m =
    -- Si son adversaire a deja gagne, retourne 0
    if (reachedEndLine board 1 n (getOpponent player) || Map.null board) then 0 
    -- Sinon, calculer la configuration du jeu, initialiser un map vide comme le map des configurations
    else head $ fromJust $ Data.Map.lookup (GameState board player) (calculConfigGame (GameState board player) n m Data.Map.empty)


{-  calculConfigGame : fonction calculer les configuration d'un jeu donne
    @param : GameState : contient   - board : la board d'un jeu, avec les pions et sa position [(position, pion)]
                                    - player : soit White soit Black
             n : hauteur de la board
             m : largeur de la board
             mapConfig : map contient des GameState et sa configuration
    @return : un map des configurations du jeu type Map GameState [Int]
-}
calculConfigGame :: GameState -> Int -> Int -> Map GameState [Int] -> Map GameState [Int]
calculConfigGame (GameState board player) n m mapConfig = 
    -- Si son adversaire a deja gagne, retourne 0
    if (reachedEndLine board 1 n (getOpponent player)) then Map.insert (GameState board player) [0] mapConfig
    else 
        if (willReachedEndLine board 2 (n-1) player) then Map.insert (GameState board player) [1] mapConfig
        else 
            -- Sinon, initialiser la liste des config de cet état = ajouter (cet état,liste vide) dans mapConfig
            -- calculer la configuration de chaque pion de ce joueur
            let newMapConfig = calculConfigPion board n m player 0 (Map.insert (GameState board player) [] mapConfig)
                -- calculer la configuration de cet état, selon les config de ses pions, calculer par le formule calculValeur
                resultatConfig = fromJust $ Map.lookup (GameState board player) newMapConfig
                resultat = [calculValeur resultatConfig]
            in Map.insert (GameState board player) resultat newMapConfig
    

{-  calculConfigPion : fonction calculer des configuration d'un pion donne
selon son choix : - soit manger son adversaire en diagonale gauche
                  - soit manger son adversaire en diagonale droite
                  - soit avancer un pas
    @param : board : la board d'un jeu, avec les pions et sa position [(position, pion)]
             n : hauteur de la board
             m : largeur de la board
             player : le joueur, soit White soit Black
             ((x,y), p) : un pion, avec - sa position (x,y) 
                                        - son p soit (Just W), soit (Just B)
             mapConfig : map contient des GameState et sa configuration    
    @return : map contient des GameState et sa configuration, c'est le mapConfig, en ajoutant les choix du pion
-}
calculConfigPion :: Board -> Int -> Int -> Player -> Int -> Map GameState [Int] -> Map GameState [Int]
-- Calculer les moves du 1er pion de la liste
calculConfigPion board n m player indice mapConfig =
    -- S'il n'y a plus de pions, mapConfig ne change pas
    if (length board==indice) then mapConfig
    else 
    let ((x,y), p) = Map.elemAt indice board
    in
        -- Si ce pion n'appartient pas au joueur, continuer avec le reste de la liste des pions, mapConfig ne change pas 
        if (fromJust p /= getPieceType player) then calculConfigPion board n m player (indice+1) mapConfig
        else  
            let newY = (moveY (getPieceType player)) y 1    -- calculer le nouveau y selon le player, si White : go up, si Black : go down
                pOpponent = getOpponentPiece p              -- recuperer son adversaire
                -- ajouter l'état si il mange son adversaire en diagonale gauche, et sa config
                mapConfigLeft  = calculConfigMove board n m player (x-1,newY) (Just pOpponent) (x,y) ((x-1,newY), p) mapConfig
                
                -- ajouter l'état si il mange son adversaire en diagonale droit, et sa config
                mapConfigRight = calculConfigMove board n m player (x+1,newY) (Just pOpponent) (x,y) ((x+1,newY), p) mapConfigLeft
                
                -- ajouter l'état si il avance 1 pas, et sa config
                mapConfigAjoute = calculConfigMove board n m player (x,newY)  (Nothing)        (x,y) ((x,newY), p)   mapConfigRight
                listConfig = fromJust $ Map.lookup (GameState board player) mapConfigAjoute
                            -- si dans la liste config de cet état, il y a des valeurs négatives et contient 0, donc arrête à chercher, parce que la config=1 en tout cas
                newMapConfig | (calculValeur listConfig == 1) = mapConfigAjoute
                                -- si non, continuer avec le reste de la liste des pions
                             | otherwise = calculConfigPion board n m player (indice+1) mapConfigAjoute
            in newMapConfig



{-  reachedEndLine : fonction verifier si un des deux joyeurs est gagner, cad un des deux atteint la derniere ligne
    @param : board : la board d'un jeu, avec les pions et sa position [(position, pion)]
             n : hauteur de la board
             player : le joueur, soit White soit Black
    @return : True si un des deux joueur est gagne, False sinon
-}
reachedEndLine :: Board -> Int -> Int -> Player -> Bool
reachedEndLine board n0 n player    
    | player == White = whiteOnEndLine n0 n           -- Si player White, verifier la derniere ligne par la fonction WhiteOnEndLine
    | otherwise       = blackOnFirstLine n n0         -- Si player Black, verifier la 1er ligne par la fonction blackOnFirstLine
        where whiteOnEndLine indice n  = if (indice>n) then False   --return True s'il trouve un pion blanc a la derniere ligne
                                         else let trouve = Map.lookup (indice,n) board
                                              in if (trouve == Just (Just W)) then True
                                                 else whiteOnEndLine (indice+1) n
              blackOnFirstLine indice n = if (indice<n) then False   --return True s'il trouve un pion noir a la premiere ligne
                                          else let trouve = Map.lookup (indice,n) board
                                               in if (trouve == Just (Just B)) then True
                                                  else blackOnFirstLine (indice-1) n

willReachedEndLine :: Board -> Int -> Int -> Player -> Bool
willReachedEndLine board n0 n player
    | player == White = whiteOnEndLine n0 n           -- Si player White, verifier la derniere ligne par la fonction WhiteOnEndLine
    | otherwise       = blackOnFirstLine n n0         -- Si player Black, verifier la 1er ligne par la fonction blackOnFirstLine
        where whiteOnEndLine indice n  = if (indice>n) then False   --return True s'il trouve un pion blanc a la derniere ligne
                                         else let trouve = Map.lookup (indice,n) board
                                                  isNothing = Nothing == Map.lookup (indice,n+1) board
                                                  hasLeft = Just (Just B) == Map.lookup (indice-1,n+1) board
                                                  hasRight = Just (Just B) == Map.lookup (indice+1,n+1) board
                                                  condition = isNothing || hasLeft || hasRight
                                              in if (trouve == Just (Just W) && condition) then True
                                                 else whiteOnEndLine (indice+1) n
              blackOnFirstLine indice n = if (indice<n) then False   --return True s'il trouve un pion noir a la premiere ligne
                                          else let trouve = Map.lookup (indice,n) board
                                                   isNothing = Nothing == Map.lookup (indice,n-1) board
                                                   hasLeft = Just (Just W) == Map.lookup (indice-1,n-1) board
                                                   hasRight = Just (Just W) == Map.lookup (indice+1,n-1) board
                                                   condition = isNothing || hasLeft || hasRight
                                               in if (trouve == Just (Just B) && condition) then True
                                                  else blackOnFirstLine (indice-1) n

                                                    
{-  calculConfigMove : fonction calculer le configuration a partir un pas du pion
    @param : board : la board d'un jeu, avec les pions et sa position [(position, pion)]
             n : hauteur de la board
             m : largeur de la board
             player : le joueur, soit White soit Black
             (x2,y2) : la position pour chercher un pion
             pieceToCompare : valeur d4'un pion pour comparer avec le resultat de la recherche
             listToDelete : liste des pions a supprimer de la board
             pion1 : pion a remplacer
             pion2 : pion remplacant
             mapConfig : map contient des GameState et sa configuration    
    @return : mapConfig : map contient des GameState et sa configuration
-}
calculConfigMove :: Board -> Int -> Int -> Player -> (Int,Int) -> Maybe (Maybe Piece) -> Pos -> Square -> Map GameState [Int] -> Map GameState [Int]
calculConfigMove board n m player (x,y) pieceToCompare (x0,y0) (pos2, pion2) mapConfig = 
    if (x>m || x<1 || ((Map.lookup (x,y) board) /= pieceToCompare)) then mapConfig
    {- si son x est hors de la board, soit x2<1 ou x2>largeur, retourne 0
       si a cette position, la valeur trouve n'est pas comme expecte, retourne 0
                            valeur expecte : - si c'est un move diagonale gauche : souhaite avoir son adversaire a cette position
                                             - si c'est un move diagonale droit : souhaite avoir son adversaire a cette position
                                             - si c'est un move avancer : souhaite n'avoir rien a cette position
    -}
    else 
            -- Si c'est un move diagonale, supprimer le pion adversaire de cette position pour preparer le move
        let newBoard0 = Map.delete (x0,y0) board
            newBoard = Map.insert (x,y) pion2 newBoard0            -- mettre le pion dans la position choisie par la fonction replacePion
            newPlayer = (getOpponent player)
            newGameState = GameState newBoard newPlayer  -- newGameState est un état avec le move du joueur, et le tour pour son adversaire
            justConfig = Map.lookup newGameState mapConfig
                             -- si ce nouvel état est trouvable dans le map des configs
            finalMapConfig = if (justConfig /= Nothing) then
                                let config = fromJust justConfig
                                    f x = if x /= [] then Just (config++x) else Just config
                                   -- ajouter cet config dans la liste de cet état (avant newGameState)
                                in Map.update f (GameState board player) mapConfig
                            else 
                                    -- sinon, calculer le config du nouvel état
                                let newMapConfig = calculConfigGame newGameState n m mapConfig
                                    newConfig = fromJust $ Map.lookup newGameState newMapConfig
                                    f x = if x /= [] then Just (newConfig++x) else Just newConfig
                                
                                   -- ajouter cet config dans la liste de cet état (avant newGameState)
                                in Map.update f (GameState board player) newMapConfig
        in  finalMapConfig

------------------- Test temps d'execution ------------------------
t9 = ["     ", "pp pp", "PPp  ","   P ", "    P"]
bt9 = readBoard 5 t9
test9 = calculConfig (GameState bt9 White) 5 5