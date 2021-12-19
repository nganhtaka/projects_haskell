module ParserCours where

import Prelude hiding (pure, (>>=), (>>))
import Data.Char (isDigit)

-- type Parser = String -> ArbreDeSyntaxeAbstraite (AST)
-- type Parser = String -> (ArbreDeSyntaxeAbstraite, String)
-- type Parser a = String -> (a, String)
-- type Parser a = String -> Maybe (a, String)

newtype Parser a = MkParser (String -> Resultat a)
type Resultat a  = Maybe (a, String)

runParser :: Parser a -> String -> Resultat a
runParser (MkParser f) cs = f cs

-- Briques de base

unCaractereQuelconque :: Parser Char
unCaractereQuelconque = MkParser f
    where f (c:cs) = Just (c, cs)
          f ""     = Nothing

-- Le parseur qui échoue toujours
empty :: Parser a
empty = MkParser (const Nothing)

-- Le parseur qui réussit toujours sans consommer de caractère de
-- l’entrée
pure :: a -> Parser a
pure x = MkParser f
    where f cs = Just (x, cs)


-- Combinateurs

-- Alternative
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = MkParser h
    where h cs = case runParser p1 cs of
                   Nothing -> runParser p2 cs
                   res     -> res

-- Séquence
-- Penser à zip vs zipWith
-- Parser a -> Parser b -> Parser (a, b)
-- (a -> b -> c) -> Parser a -> Parser b -> Parser c
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = MkParser g
    where g cs = case runParser p cs of
                   Nothing       -> Nothing
                   Just (x, cs') -> runParser (f x) cs'


-- Maintenant, plus besoin de regarder comment est déclaré le type des
-- Parser

carQuand :: (Char -> Bool) -> Parser Char
carQuand cond = unCaractereQuelconque >>= filtre
    where filtre c | cond c    = pure c
                   | otherwise = empty

exP1 :: Parser Char
exP1 = carQuand isDigit

ex1 = runParser exP1 "abc"
ex2 = runParser exP1 "123"
ex3 = runParser exP1 ""

car :: Char -> Parser Char
-- car c = unCaractereQuelconque >>= filtre
--     where filtre c' | c == c'   = pure c
--                     | otherwise = empty
car c = carQuand (\c' -> c == c')

chaine :: String -> Parser String
chaine "" = pure ""
chaine (c:cs) = car c       >>= \_ ->
                chaine cs   >>= \_ ->
                pure (c:cs)

chiffre :: Parser Char
chiffre = carQuand isDigit

nombre :: Parser String
-- nombre = chiffre >>= (\c ->
--             ( nombre >>= (\cs ->
--               pure (c:cs)) )
--             <|> pure [c] )
nombre = chiffre        >>= \c ->
         ( nombre       >>= \cs ->
           pure (c:cs) )
         <|> pure [c]

nombreI :: Parser Integer
-- nombreI = nombre >>= (\cs -> pure (read cs))
nombreI = nombre >>= (pure . read)

ex4 = runParser nombre ""
ex5 = runParser nombre "abc"
ex6 = runParser nombre "123"
ex7 = runParser nombre "123abc"
ex8 = runParser nombre "123abc456"

ex9  = runParser nombreI ""
ex10 = runParser nombreI "abc"
ex11 = runParser nombreI "123"
ex12 = runParser nombreI "123abc"
ex13 = runParser nombreI "123abc456"

-- Analyseur d’expressions
-- Nombre :: = Chiffres +
-- Expression :: = Nombre '+' Expression | Nombre

data Expression a = Nombre a
                  | Add (Expression a) (Expression a)
    deriving Show

type ParserExpression = Parser (Expression Integer)

(>>) :: Parser a -> Parser b -> Parser b
p1 >> p2 = p1 >>= \_ -> p2

nombreExpr :: ParserExpression
nombreExpr = nombreI >>= (pure . Nombre)

expression :: ParserExpression
expression = ( nombreExpr >>= \n ->
               car '+'    >>
               expression >>= \e ->
               pure (Add n e) )
           <|> nombreExpr

ex14 = runParser expression ""
ex15 = runParser expression "1"
ex16 = runParser expression "1+2"
ex17 = runParser expression "1+2+3"

eval :: Expression Integer -> Integer
eval (Nombre n)  = n
eval (Add e1 e2) = eval e1 + eval e2
