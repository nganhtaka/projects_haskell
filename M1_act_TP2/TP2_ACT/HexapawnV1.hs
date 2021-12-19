-- -- -- -- -- -- -- --
--    Hexapawn v1    --
-- -- -- -- -- -- -- --
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import Data.Maybe (fromJust)
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as M

type Pos        = (Int, Int)
type Square     = (Pos, Piece)
type Board      = [Square]
data Piece      = B | W deriving (Show, Eq, Ord)
data Player     = Black | White deriving (Show, Eq, Ord)
data GameState  = GameState Board Player deriving (Show)
instance Eq GameState where
    GameState b1 p1 == GameState b2 p2 = b1 == b2 && p1 == p2
instance Ord GameState where
    compare (GameState [] pl1) (GameState [] pl2) = compare pl1 pl2
    compare (GameState [] _)   (GameState (_ : _) _) = LT
    compare (GameState (((_, _), _) : _) _) (GameState [] _) = GT
    compare (GameState (((x1,y1), p1):xs1) pl1) (GameState (((x2,y2), p2):xs2) pl2) =
        if (length (((x1,y1), p1):xs1)) == (length (((x2,y2), p2):xs2)) then
            if (x1/=x2) then compare x1 x2
            else
                if (y1/=y2) then compare y1 y2
                else
                    if (p1/=p2) then compare p1 p2
                    else compare (GameState xs1 pl1) (GameState xs2 pl2)
        else compare (length (((x1,y1), p1):xs1)) (length (((x2,y2), p2):xs2))

getPieceType :: Player -> Piece
getPieceType White = W
getPieceType Black = B

getOpponent :: Player -> Player
getOpponent White = Black
getOpponent Black = White

getOpponentPiece :: Piece -> Piece
getOpponentPiece W = B
getOpponentPiece B = W

readPiece :: Char -> Maybe Piece
readPiece 'p' = Just B
readPiece 'P' = Just W
readPiece _   = Nothing

readBoard :: Int -> Int -> [String] -> Board
readBoard m n xs = concatMap (readRow m) (zip (reverse [1..n]) xs)

readRow :: Int -> (Int, String) -> [Square]
readRow m (i, row)      = map (\(pos, p) -> (pos, fromJust(p))) $ filter(\s -> snd s /= Nothing) squares
        where positions = [(x, i) | x <- [1..m]]
              safeRow   = row ++ (concat $ take (m - length row) $ repeat " ")
              squares   = zip positions (map readPiece safeRow)

main :: IO()
main = do
    contents <- getContents
    let (_n:_m:rest) = lines contents
        n            = read _n
        m            = read _m
        board        = readBoard m n rest
        value        = evaluateGameState m n (GameState board White)
    putStrLn . show $ value


memomain :: IO()
memomain = do
    contents <- getContents
    let (_n:_m:rest) = lines contents
        n            = read _n
        m            = read _m
        board        = readBoard m n rest
        value        = evaluateGameStateMemo m n (GameState board White)
    putStrLn . show $ value


-- -- -- -- ---- -- --
--  **Question 1**  --
-- -- -- -- ---- -- --
contientNegatif :: [Int] -> Bool
contientNegatif listeValeurSuccesseur = (minimum listeValeurSuccesseur) <= 0

calculValeur :: [Int] -> Int
calculValeur listeValeurSuccesseur
    | null listeValeurSuccesseur            = 0
    | contientNegatif listeValeurSuccesseur = -((maximum (filter (<=0) listeValeurSuccesseur))-1)
    | otherwise                             = -((maximum listeValeurSuccesseur)+1)


-- -- -- -- ---- -- --
--  **Question 2**  --
-- -- -- -- ---- -- --

{-
| Renvoit vrai si un joueur est arrivé au dernier ligne qui lui permettrait de gagner
-}
reachedEndLine :: Board -> Int -> Player -> Bool
reachedEndLine board n player
    | player == White = not (null whiteOnEndLine)
    | otherwise       = not (null blackOnFirstLine)
        where whiteOnEndLine   = [True | ((_,y), p) <- board, y == n && p == W]
              blackOnFirstLine = [True | ((_,y), p) <- board, y == 1 && p == B]

noMorePieces :: Board -> Player -> Bool
noMorePieces board player = null $ getPlayerPieces board player

{-
| Renvoit vrai si un joueur est arrivé à l'avant dernier ligne qui lui permettrait de gagner
-}
willReachEndLine :: GameState -> Int -> Bool
willReachEndLine (GameState board player) n
    | player == White = not (null whiteOnEndLine)
    | otherwise       = not (null blackOnFirstLine)
        where whiteOnEndLine   = [True | ((_,y), p) <- board, y == n-1 && p == W]
              blackOnFirstLine = [True | ((_,y), p) <- board, y == 2 && p == B]

{-
| @param board le jeu avec toutes les pièces
| @param player le joueur dont c'est le tour
| @return tous les pièces du jouer dont c'est le tour
-}
getPlayerPieces :: Board -> Player -> [Square]
getPlayerPieces board player = filter (isTeamPiece) board
    where isTeamPiece square = snd square == getPieceType player

{-
| @param (GameState board player) la configuration du jeu
| @param n le hauteur du jeu
| @return True si le jeu est terminé, sinon False
-}
isFinished :: GameState -> Int -> Bool
isFinished (GameState board player) n = noMorePieces board player || noMorePieces board other
                                     || reachedEndLine board n player || reachedEndLine board n other
                                    where other = getOpponent player

moveY :: Piece -> (Int -> Int -> Int)
moveY W = (+)
moveY B = (-)

{-
| Renvoit le nouveau configuration du jeu suivant un coup en avant par une pièce du joueur dont c'est le tour
| sinon renvoit Nothing si la case en avant est occupée
| @param (GameState board player) la configuration du jeu
| @param ((x,y), p) une case du jeu avec (x,y) le coordonné et p le type de pièce
| @return une liste contenant le nouveau configuration du jeu la pièce p dans la case a été joué
-}
advanceState :: GameState -> Square -> [GameState]
advanceState (GameState board player) ((x,y), p)
    | lookup newPos board == Nothing = let newBoard = sortBy (comparing fst) ((newPos, p):(delete ((x,y), p) board))
                                       in [GameState newBoard (getOpponent player)]
    | otherwise                      = []
    where newPos = (x, moveY p y 1)

{-
| Renvoit le nouveau configuration du jeu suivant une attaque sur une case en diagonale gauche
| sinon renvoit Nothing si aucune attaque ne peut être fait sur la case gauche en diagonale
| @param (GameState board player) la configuration du jeu
| @param ((x,y), p) une case du jeu avec (x,y) le coordonné et p le type de pièce
| @return une liste contenant le nouveau configuration du jeu la pièce p dans la case a été joué
-}
leftAttackState :: GameState -> Square -> [GameState]
leftAttackState (GameState board player) ((x,y), p)
    | x == 1                                 = []
    | lookup newPos board == Just otherPiece = let newBoard = sortBy (comparing fst) ((newPos, p): filter(\pawn -> pawn /= ((x,y), p) && pawn /= (newPos, otherPiece)) board)
                                               in [GameState newBoard (getOpponent player)]
    | otherwise                              = []
    where newPos     = (x-1, moveY p y 1)
          otherPiece = getOpponentPiece p

{-
| Renvoit le nouveau configuration du jeu suivant une attaque sur une case en diagonale droite
| sinon renvoit Nothing si aucune attaque ne peut être fait sur la case droite en diagonale
| @param (GameState board player) la configuration du jeu
| @param ((x,y), p) une case du jeu avec (x,y) le coordonné et p le type de pièce
| @return une liste contenant le nouveau configuration du jeu la pièce p dans la case a été joué
-}
rightAttackState :: GameState -> Int -> Square -> [GameState]
rightAttackState (GameState board player) m ((x,y), p)
    | x == m                                 = []
    | lookup newPos board == Just otherPiece = let newBoard = sortBy (comparing fst) ((newPos, p): filter(\pawn -> pawn /= ((x,y), p) && pawn /= (newPos, otherPiece)) board)
                                               in [GameState newBoard (getOpponent player)]
    | otherwise                              = []
    where newPos     = (x+1, moveY p y 1)
          otherPiece = getOpponentPiece p

{-
| Renvoit une liste de toutes les configurations possibles après avoir joué chacun des coups possibles par le joueur dont
| c'est le tour
| @param config la configuration du jeu
| @param m la longeur de la carte du jeu
| @return une liste contenant les nouveaux configurations du jeu
-}
possibleGameStates :: GameState -> Int -> [GameState]
possibleGameStates config m = concatMap getNewStates (getPlayerPieces board player)
    where GameState board player = config
          getNewStates pawn = advanceState config pawn ++ leftAttackState config pawn ++ rightAttackState config m pawn

{-
| Renvoit la valeur d'une configuration de jeu
| @param m la longeur de la carte
| @param n la hauteur de la carte
| @param config la configuration
-}
evaluateGameState :: Int -> Int -> GameState -> Int
evaluateGameState m n config
    | isFinished config n = 0
    | otherwise           = calculValeur $ map (evaluateGameState m n) (possibleGameStates config m)


-- -- -- -- ---- -- --
--  **Question 3**  --
-- -- -- -- ---- -- --

{-
| Version memoization de la version naive qui ne passe pas sur les tests 6 et 7 sur la plateforme
| Version memoization avec un Map pour sauvegarder le GameState et sa valeur dans le Map
| Si le jeu est terminé il le sauvegarde dans le map avec sa valeur, sinon il calcule sa valeur selon ses configurations | successives
-}

{-
| Renvoit la valeur d'une configuration de jeu
| @param m la longeur de la carte
| @param n la hauteur de la carte
| @param config la configuration
-}
evaluateGameStateMemo :: Int -> Int -> GameState -> Int
evaluateGameStateMemo m n config =
    if (isFinished config n)
        then 0
        else fromJust $ M.lookup config (evalGameState m n M.empty config)


{-
| Renvoit un map contenant les configurations qu'on a déjà rencontrés ainsi que leurs valeurs
| @param m la longeur de la carte
| @param n la hauteur de la carte
| @param memoMap le Map qui associe les configurations qu'on a déjà rencontrés avec leurs valeurs
| @param config la configuration
-}
evalGameState :: Int -> Int -> Map GameState Int -> GameState -> Map GameState Int
evalGameState m n memoMap config
    | M.member config memoMap   = memoMap
    | isFinished config n       = M.insert config 0 memoMap
    | willReachEndLine config n = M.insert config 1 memoMap
    | otherwise                 = M.insert config value newMemoMap
            where
                (values, newMemoMap) = foldr step ([], memoMap) (possibleGameStates config m)
                step gameS (xs, accMap) | M.member gameS accMap = (M.lookup gameS accMap : xs, accMap)
                                        | otherwise             = let newMap = evalGameState m n accMap gameS
                                                                  in (M.lookup gameS newMap : xs, newMap)
                value = calculValeur $ map fromJust values


------------------- Test temps d'execution ------------------------
t9 = ["     ", "pp pp", "PPp  ","   P ", "    P"]
bt9 = readBoard 5 5 t9
test9 = evaluateGameState 5 5 (GameState bt9 White)