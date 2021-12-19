module EffetsDeBord where

import           Control.Applicative
import           Data.Char           (isDigit)
import           Data.Maybe          (fromJust)

import           ParserCours         (eval, expression, runParser)
import           Control.Monad
import           System.IO.Error

instance MonadPlus IO where
    mzero       = ioError (userError "mzero")
    m `mplus` n = m `catchIOError` \ _ -> n

instance Alternative IO where
    empty = mzero
    (<|>) = mplus
    

affiche :: String -> ()
affiche = undefined

ex1 = [ affiche "123", affiche "456" ]

id' :: a -> a
id' x = let _ = putStr "boum"
        in  x

-- putStr :: String -> IO ()
-- IO a : type d’une _action_ qui, quand elle sera effectuée, pourra
-- avoir un effet de bord et produire une valeur de type a

getChar' :: IO Char
getChar' = getChar

putChar' :: Char -> IO ()
putChar' = putChar

echo :: IO ()
echo = getChar >>= \c -> putChar c

echo2' :: IO ()
echo2' = getChar >>= (\c ->
            putChar c >>= (\_ ->
                putChar c))
echo2 :: IO ()
echo2 = getChar   >>= \c ->
        putChar c >>
        putChar c

echo2'' :: IO ()
echo2'' = do c <- getChar
             putChar '«'
             putChar c
             putChar c
             putChar '»'

pure' :: a -> IO a
pure' = pure

deuxCars :: IO (Char, Char)
deuxCars = getChar >>= \c1 ->
           getChar >>= \c2 ->
           pure (c1, c2)

deuxCars' :: IO (Char, Char)
deuxCars' = do c1 <- getChar
               c2 <- getChar
               pure (c1, c2)

memeCars :: IO Bool
memeCars = do c1 <- getChar
              c2 <- getChar
              pure (c1 == c2)

litLigne :: IO String
litLigne = do
    c <- getChar
    if c == '\n'
        then pure ""
        else do cs <- litLigne
                pure (c:cs)

getCharQuand :: (Char -> Bool) -> IO Char
getCharQuand cnd = getChar >>= filtre
    where filtre c | cnd c     = pure c
                   | otherwise = empty

litLigne' :: IO String
litLigne' = do c <- getCharQuand (/= '\n')
               cs <- litLigne'
               pure (c:cs)
           <|> pure ""

many' :: IO a -> IO [a]
many' a = do r <- a
             rs <- many' a
             pure (r:rs)
         <|> pure []

litLigne'' :: IO String
litLigne'' = many' (getCharQuand (/= '\n'))

-- Comment faire des programmes
-- Découpe entre la partie pure (vrai code dur à écrire, qui sera plus
-- facile à tester) et la partie avec des effets de bord, aussi
-- limitée que possible
interact' :: (String -> String) -> IO ()
interact' f = do entree <- many getChar
                 putStr (f entree)

repeteJusquA :: IO Bool -> IO () -> IO ()
repeteJusquA arret act = do stop <- arret
                            if stop
                                then pure ()
                                else do act
                                        repeteJusquA arret act

eternellement :: IO () -> IO ()
eternellement a = do a
                     eternellement a

putStr' :: String -> IO ()
putStr'     [] = pure ()
putStr' (c:cs) = do putChar c
                    putStr' cs

putStr'' :: String -> IO ()
putStr'' cs = mapM_ putChar cs

main :: IO ()
-- main = do l <- litLigne
--           putStrLn l
-- main = interact' reverse
main = interact evalExpr
    where evalExpr = show . eval . fst . fromJust . runParser expression
