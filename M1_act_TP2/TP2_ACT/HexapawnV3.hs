-- -- -- -- -- -- -- --
--    Hexapawn v3    --
-- -- -- -- -- -- -- --
module Main where
import Data.Maybe (fromJust)
import Data.List (delete)
import Data.Map (lookup, empty, Map)
import qualified Data.Map as Map

-- B : Black, W : White
data Player    = Black | White deriving (Show, Eq, Ord)
data Piece     = B | W deriving (Show, Eq, Ord)
type Pos       = (Int, Int)
type Square    = (Pos, Maybe Piece)
type Board     = [Square]
data GameState = GameState Board Player deriving (Show, Ord)
instance Eq GameState where
    GameState b1 p1 == GameState b2 p2 = b1 == b2 && p1 == p2

main :: IO()
main = do
    contents <- getContents
    let (_n:_m:rest) = lines contents
        n            = read _n
        m            = read _m
        board        = readBoard m (zip (reverse [1..n]) rest)
        value        = calculConfig (GameState board White) n m
    putStrLn . show $ value

readBoard :: Int -> [(Int, String)] -> Board
readBoard m xs = concatMap readRow xs
    where readRow row = filter isSquareOccupied (generate m row)

generate :: Int -> (Int, String) -> [Square]
generate m (n, row) = zip [(x, n) | x <- [1..m]] squares
    where squares = map readPiece safeRow
          safeRow = row ++ (concat $ take (m - length row) $ repeat " ")

isSquareOccupied :: Square -> Bool
isSquareOccupied (_, p) = p /= Nothing

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
calculValeur [x] = if (x<=0) then -(x-1) else -(x+1)
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
    if (reachedEndLine board n (getOpponent player) || board==[]) then 0
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
    if (reachedEndLine board n (getOpponent player)) then Map.insert (GameState board player) [0] mapConfig
    else
        if (willReachedEndLine board (n-1) player) then Map.insert (GameState board player) [1] mapConfig
        else
            -- Sinon, initialiser la liste des config de cet état = ajouter (cet état,liste vide) dans mapConfig
            -- calculer la configuration de chaque pion de ce joueur
            let newMapConfig = calculConfigPion board n m player board (Map.insert (GameState board player) [] mapConfig)
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
calculConfigPion :: Board -> Int -> Int -> Player -> [Square] -> Map GameState [Int] -> Map GameState [Int]
-- S'il n'y a plus de pions, mapConfig ne change pas
calculConfigPion _ _ _ _ [] mapConfig = mapConfig

-- Si non, pour calculer les moves du 1er pion de la liste
calculConfigPion board n m player (((x,y), p):xs) mapConfig =
    -- Si ce pion n'appartient pas au joueur, continuer avec le reste de la liste des pions, mapConfig ne change pas
    if (fromJust p /= getPieceType player) then calculConfigPion board n m player xs mapConfig
    else
        let newY = (moveY (getPieceType player)) y 1    -- calculer le nouveau y selon le player, si White : go up, si Black : go down
            pOpponent = getOpponentPiece p              -- recuperer son adversaire
            -- ajouter l'état si il mange son adversaire en diagonale gauche, et sa config
            mapConfigLeft  = calculConfigMove board n m player (x-1,newY) (Just pOpponent) [((x-1,newY), pOpponent)] ((x,y),p) ((x-1,newY), p) mapConfig

            -- ajouter l'état si il mange son adversaire en diagonale droit, et sa config
            mapConfigRight = calculConfigMove board n m player (x+1,newY) (Just pOpponent) [((x+1,newY), pOpponent)] ((x,y),p) ((x+1,newY), p) mapConfigLeft

            -- ajouter l'état si il avance 1 pas, et sa config
            mapConfigAjoute = calculConfigMove board n m player (x,newY)  (Nothing)         []                        ((x,y),p) ((x,newY), p)  mapConfigRight

            listConfig = fromJust $ Map.lookup (GameState board player) mapConfigAjoute
                            -- si dans la liste config de cet état, il y a des valeurs négatives et contient 0, donc arrête à chercher, parce que la config=1 en tout cas
            newMapConfig | (calculValeur listConfig == 1) = mapConfigAjoute
                            -- si non, continuer avec le reste de la liste des pions
                         | otherwise = calculConfigPion board n m player xs mapConfigAjoute
        in newMapConfig



{-  reachedEndLine : fonction verifier si un des deux joyeurs est gagner, cad un des deux atteint la derniere ligne
    @param : board : la board d'un jeu, avec les pions et sa position [(position, pion)]
             n : hauteur de la board
             player : le joueur, soit White soit Black
    @return : True si un des deux joueur est gagne, False sinon
-}
reachedEndLine :: Board -> Int -> Player -> Bool
reachedEndLine board n player
    | player == White = not (null whiteOnEndLine)           -- Si player White, verifier la derniere ligne par la fonction WhiteOnEndLine
    | otherwise       = not (null blackOnFirstLine)         -- Si player Black, verifier la 1er ligne par la fonction blackOnFirstLine
        where whiteOnEndLine   = [True | ((_,y), p) <- board, y == n && fromJust(p) == W]   --return True s'il trouve un pion blanc a la derniere ligne
              blackOnFirstLine = [True | ((_,y), p) <- board, y == 1 && fromJust(p) == B]   --return True s'il trouve un pion noir a la premiere ligne


willReachedEndLine :: Board -> Int -> Player -> Bool
willReachedEndLine board n player
    | player == White = whiteOnEndLine 2 n           -- Si player White, verifier la derniere ligne par la fonction WhiteOnEndLine
    | otherwise       = blackOnFirstLine n 2         -- Si player Black, verifier la 1er ligne par la fonction blackOnFirstLine
        where whiteOnEndLine indice n  = if (indice>n) then False   --return True s'il trouve un pion blanc a la derniere ligne
                                        else let trouve = Prelude.lookup (indice,n) board
                                                 isNothing = Nothing == Prelude.lookup (indice,n+1) board
                                                 hasLeft = Just (Just B) == Prelude.lookup (indice-1,n+1) board
                                                 hasRight = Just (Just B) == Prelude.lookup (indice+1,n+1) board
                                                 condition = isNothing || hasLeft || hasRight
                                             in if (trouve == Just (Just W) && condition) then True
                                                else whiteOnEndLine (indice+1) n
              blackOnFirstLine indice n = if (indice<n) then False   --return True s'il trouve un pion noir a la premiere ligne
                                          else let trouve = Prelude.lookup (indice,n) board
                                                   isNothing = Nothing == Prelude.lookup (indice,n-1) board
                                                   hasLeft = Just (Just W) == Prelude.lookup (indice-1,n-1) board
                                                   hasRight = Just (Just W) == Prelude.lookup (indice+1,n-1) board
                                                   condition = isNothing || hasLeft || hasRight
                                               in if (trouve == Just (Just B) && condition) then True
                                                  else blackOnFirstLine (indice-1) n

{-  replacePion : fonction replace un pion par un autre pion dans la board donne
    @param : pion1 : pion remplace
             pion2 : pion remplacant
             board : la board d'un jeu, avec les pions et sa position [(position, pion)]
    @return : une nouvelle board avec le pion deja remplace
-}
replacePion :: Square -> Square -> Board -> Board
replacePion pion1 pion2 = map (\x -> if (pion1 == x) then pion2 else x) --verifier dans la liste, s'il y a le pion, remplace le



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
calculConfigMove :: Board -> Int -> Int -> Player -> (Int,Int) -> Maybe (Maybe Piece) -> [Square] -> Square -> Square -> Map GameState [Int] -> Map GameState [Int]
calculConfigMove board n m player (x2,y2) pieceToCompare listToDelete pion1 pion2 mapConfig =
    if (x2>m || x2<1 || ((Prelude.lookup (x2,y2) board) /= pieceToCompare)) then mapConfig
    {- si son x est hors de la board, soit x2<1 ou x2>largeur, retourne 0
       si a cette position, la valeur trouve n'est pas comme expecte, retourne 0
                            valeur expecte : - si c'est un move diagonale gauche : souhaite avoir son adversaire a cette position
                                             - si c'est un move diagonale droit : souhaite avoir son adversaire a cette position
                                             - si c'est un move avancer : souhaite n'avoir rien a cette position
    -}
    else
            -- Si c'est un move diagonale, supprimer le pion adversaire de cette position pour preparer le move
        let newBoard0 | (not $ null $ listToDelete) = delete (head listToDelete) board
                      | otherwise = board
            newBoard = replacePion pion1 pion2 newBoard0            -- mettre le pion dans la position choisie par la fonction replacePion
            newGameState = GameState newBoard (getOpponent player)  -- newGameState est un état avec le move du joueur, et le tour pour son adversaire
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


-------------- Test temps d'execution --------------------
t9 = ["     ", "pp pp", "PPp  ","   P ", "    P"]
bt9 = readBoard 5 (zip (reverse [1..5]) t9)
test9 = calculConfig (GameState bt9 White) 5 5