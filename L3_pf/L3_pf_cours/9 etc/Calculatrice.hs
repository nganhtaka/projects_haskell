module Calculatrice where

-- Où l’on parle de comment écrire du code de façon assez légère même
-- dans le cas où on veut pouvoir retourner et propager des erreurs,
-- donc avec Maybe

-- On parle des classes de type : Functor, Applicative et Monad

import Control.Applicative

import Parser

data Expression = Nombre Integer
                | Add Expression Expression
                | Soust Expression Expression
                | Mult Expression Expression
                | Div Expression Expression
    deriving (Show, Eq, Read)

evalue :: Expression -> Maybe Integer
evalue (Nombre n ) = Just n
evalue (Add   e1 e2) = (+) <$> evalue e1 <*> evalue e2
evalue (Soust e1 e2) = (-) <$> evalue e1 <*> evalue e2
evalue (Mult  e1 e2) = (*) <$> evalue e1 <*> evalue e2
-- evalue (Div   e1 e2) = case evalue e2 of
--                          Nothing -> Nothing
--                          Just 0  -> Nothing
--                          Just v2 -> case evalue e1 of
--                                       Nothing -> Nothing
--                                       Just v1 -> Just (v1 `div` v2)
-- evalue (Div   e1 e2) = evalue e2 >>= \v2 ->
--                          case v2 of
--                            0 -> Nothing
--                            _ -> evalue e1 >>= \v1 -> pure (v1 `div` v2)
-- evalue (Div   e1 e2) = evalue e2 >>= \v2 ->
--                          case v2 of
--                            0 -> Nothing
--                            _ -> (`div` v2) <$> evalue e1
evalue (Div   e1 e2) = do v2 <- evalue e2
                          case v2 of
                            0 -> Nothing
                            _ -> (`div` v2) <$> evalue e1

-- Dans les parsers, resultat :: Resultat a -> a
resultat' :: Resultat a -> Maybe a
resultat' (Just (r, _)) = Just r
resultat' Nothing       = Nothing

sousJust :: (a -> b) -> Maybe a -> Maybe b
sousJust _ Nothing  = Nothing
sousJust f (Just a) = Just (f a)

sousJust2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
sousJust2 f (Just a) (Just b) = Just (f a b)
sousJust2 _       _        _  = Nothing

-- app s’appelle <*> dans la classe (Functor) Applicative
app :: Maybe (a -> b) -> Maybe a -> Maybe b
app (Just f) (Just arg) = Just (f arg)
app _        _          = Nothing

sousJust2' :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- sousJust2' f a b = (fmap f a) <*> b
sousJust2' f a b = f <$> a <*> b
-- comparer avec   f     a     b

resultat'' = sousJust fst

-- instance Functor Maybe where
--     fmap = sousJust

applist :: [a -> b] -> [a] -> [b]
applist = zipWith (\f a -> f a)
-- applist = zipWith ($)
-- applist (f:fs) (a:as) = f a : applist fs as
-- applist _      _      = []

applist' (f:fs) as = map f as ++ applist' fs as
applist' []     _  = []

ex1 = [id, (+1000)] <*> [1..3]
ex2 = ZipList [id, (+1000)] <*> ZipList [1..3]

paires :: Monad m => m a1 -> m a2 -> m (a1, a2)
paires mx my = do
    x <- mx
    y <- my
    pure (x,y)


-- Un cas d’usage des monades : les monades d’état, quand on veut
-- passer implicitement un état de proche en proche dans une séquence
-- d’opérations
--
-- Voir le module Control.Monad.State
-- (Parser est un cas assez complexe de monade d’état, où l’état est
-- la String représentant l’entrée pas encore analysée ; c’est une
-- version complexe parce que les parseurs peuvent échouer, donc on a
-- un Maybe en plus)
type Etat = Integer
newtype MonadEtat a = ME (Etat -> (a, Etat))

(>>=) :: MonadEtat a -> (a -> MonadEtat b) -> MonadEtat b
ME f >>= g = ME f'
    where f' e = let (x, e') = f e
                     ME g'   = g x
                 in  g' e'
