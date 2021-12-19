{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interprete where

import Parser
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import System.IO
import Control.Monad (unless, liftM, ap)

-- -- -- -- -- -- --
-- Interprétation --
-- -- -- -- -- -- --

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)

-- -- -- -- -- -- -- -- -- --
-- Analyse proprement dite --
-- -- -- -- -- -- -- -- -- --

-- Q1.
espacesP :: Parser ()
espacesP = many (carQuand (`elem` " ")) >> return ()


-- Q2.
nomP :: Parser Nom
nomP =  some (carQuand (`elem` ['a'..'z'])) >>= \n ->
        espacesP >>= \_ -> return n

nomP' :: Parser Nom
nomP' = do  n <- some (carQuand (`elem` ['a'..'z']))
            espacesP
            return n


-- Q3.
varP :: Parser Expression
varP = do n <- nomP
          return (Var n)

varP' :: Parser Expression
varP' = nomP >>= \n -> return (Var n)


-- Q4.
applique :: [Expression] -> Expression
applique []      = undefined
applique (e1:ex) = foldl (App) e1 ex

applique' :: [Expression] -> Expression
applique' []       = undefined
applique' [e]      = e
applique' [e1, e2] = App e1 e2
applique' xs       = App (applique' (init xs)) (last xs)


-- Q5, Q7, Q8, Q10.

exprP :: Parser Expression
exprP = varP
        <|> lambdaP
        <|> exprParentheseeP
        <|> nombreP
        <|> booleenP

exprsP :: Parser Expression
exprsP = do expression <- some exprP
            return (applique expression)


-- Q6.
lambdaP :: Parser Expression
lambdaP = do _ <- car '\\' <|> car 'λ'
             espacesP
             n <- nomP
             espacesP
             _ <- chaine "->"
             espacesP
             e <- exprsP
             return (Lam n e)


-- Q8.
exprParentheseeP :: Parser Expression
exprParentheseeP = do _ <- car '('
                      espacesP
                      n <- exprsP
                      _ <- car ')'
                      espacesP
                      return n


-- Q9.
nombreP :: Parser Expression
nombreP = do n <- some (carQuand isDigit)
             espacesP
             return (Lit(Entier(read n)))


-- Q10.
booleenP :: Parser Expression
booleenP = do n <- chaine "True" <|> chaine "False"
              espacesP
              return (Lit(Bool(read n)))


-- Q11.

expressionP :: Parser Expression
expressionP= espacesP >>=
                \_ -> exprsP >>=
                \x -> return x

expressionP' :: Parser Expression
expressionP' = espacesP >> exprsP


-- Q12.
ras :: String -> Expression
ras s = case runParser expressionP s of
             Just (e, "") -> e
             Just (_, _)  -> (error "Analyse incomplète")
             Nothing     -> (error "Erreur d’analyse syntaxique")


-- -- -- -- -- -- -- --
-- Interprète simple --
-- -- -- -- -- -- -- --

-- Q13.
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)


-- Q14.
instance Show ValeurA where
    show (VFonctionA _)          = "λ"
    show (VLitteralA (Entier n)) = show n
    show (VLitteralA (Bool b))   = show b

type Environnement a = [(Nom, a)]


-- Q15.
interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _   (Lit l)     = VLitteralA l
interpreteA env (Var x)     = fromJust (lookup x env)
interpreteA env (Lam x e)   = VFonctionA (\v -> interpreteA ((x, v):env) e)
interpreteA env (App e1 e2) = vfonction (interpreteA env e1) (interpreteA env e2)
                              where vfonction (VFonctionA f) = f
                                    vfonction _ = undefined


-- -- -- -- -- -- -- -- --
-- Quelques primitives  --
-- -- -- -- -- -- -- -- --

-- Q16.
getEntierA :: ValeurA -> Integer
getEntierA (VLitteralA (Entier n)) = n
getEntierA _                       = undefined

negA :: ValeurA
negA = VFonctionA (\v -> VLitteralA (Entier (-(getEntierA v))))


-- Q17.
addA :: ValeurA
addA = VFonctionA (\m -> (VFonctionA (\n -> VLitteralA (Entier ((getEntierA m) + (getEntierA n))))))

addA' :: ValeurA
addA' = releveBinOpEntierA (+)

subA :: ValeurA
subA = releveBinOpEntierA (-)

multA :: ValeurA
multA = releveBinOpEntierA (*)

divA :: ValeurA
divA = releveBinOpEntierA (div)

-- Q18
releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA f = VFonctionA (\m -> (VFonctionA (\n -> VLitteralA (Entier (f (getEntierA m) (getEntierA n))))))

envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if",    ifthenelseA)]

-- Q19.
getBoolA :: ValeurA -> Bool
getBoolA (VLitteralA (Bool n)) = n
getBoolA _                     = undefined

ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\a -> case getBoolA a of
                                        True -> (VFonctionA (\b -> (VFonctionA (\_ -> b))))
                                        False -> (VFonctionA (\_ -> (VFonctionA (\c -> c)))))


-- -- -- -- -- -- -- -- --
-- Boucle d'interaction --
-- -- -- -- -- -- -- -- --

-- Q20.
loop :: IO ()
loop = do
        putStr "minilang > "
        hFlush stdout
        str <- getLine
        let res = interpreteA envA (ras str)
        putStrLn (show res)
        loop
        fin <- isEOF
        unless fin loop


-- -- -- -- -- -- -- -- -- --
-- Interprète avec erreurs --
-- -- -- -- -- -- -- -- -- --

data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

-- Q21.
instance Show ValeurB where
        show (VLitteralB (Entier n)) = show n
        show (VLitteralB (Bool b))   = show b
        show (VFonctionB _)          = "λ"

-- Q22.
interpreteB :: Environnement ValeurB -> Expression -> ErrValB
interpreteB _ (Lit l)       = Right $ VLitteralB l

interpreteB env (Var x)     = case lookup x env of
                                   Just val -> Right val
                                   Nothing  -> Left ("la variable " ++ x ++ " n'est pas definie")

interpreteB env (Lam x e)   = Right $ VFonctionB (\v -> interpreteB ((x, v):env) e)

interpreteB env (App e1 e2) =
        case interpreteB env e1 of
             Left err             -> Left err
             Right (VFonctionB f) -> case interpreteB env e2 of
                                          Left err  -> Left err
                                          Right val -> (f val)
             Right b              -> Left (show b ++ " n'est pas une fonction, application impossible")

-- Q23.
{- evalB :: (Integer -> Integer -> Integer) -> ValeurB -> ValeurB -> ErrValB
evalB f a b = case a of
                   (VLitteralB (Entier n)) -> case b of
                                                   (VLitteralB (Entier m)) -> Right (VLitteralB (Entier (f n m)))
                                                   _                       -> Left (show b ++ " n'est pas un entier")
                   _                       -> Left (show a ++ " n'est pas un entier")

addB :: ValeurB
addB = VFonctionB (\m -> Right (VFonctionB (\n -> (evalB (+) m n)))) -}

addB :: ValeurB
addB = VFonctionB f
        where f (VLitteralB (Entier n)) = Right (VFonctionB g)
                        where g (VLitteralB (Entier m)) = Right (VLitteralB (Entier ((+) n m)))
                              g x                       = Left (show x ++ " n'est pas un entier")
              f x                       = Left (show x ++ " n'est pas un entier")

-- Q24.
quotB :: ValeurB
quotB = VFonctionB f
        where f (VLitteralB (Entier n)) = Right (VFonctionB g)
                        where g (VLitteralB (Entier 0)) = Left ("division par zero")
                              g (VLitteralB (Entier m)) = Right (VLitteralB (Entier (n `quot` m)))
                              g x                       = Left (show x ++ " n'est pas un entier")
              f x                       = Left (show x ++ " n'est pas un entier")


-- -- -- -- -- -- -- -- --
--  Interprète traçant  --
-- -- -- -- -- -- -- -- --
data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)


-- Q25.
instance Show ValeurC where
        show (VLitteralC (Entier n)) = show n
        show (VLitteralC (Bool b))   = show b
        show (VFonctionC _)          = "λ"


-- Q26
interpreteC :: Environnement ValeurC -> Expression -> OutValC
interpreteC _ (Lit l)       = ("", VLitteralC l)
interpreteC env (Var x)     = ("", fromJust (lookup x env))
interpreteC env (Lam x e)   = ("", VFonctionC (\v -> interpreteC ((x, v):env) e))
interpreteC env (App e1 e2) =
        case interpreteC env e1 of
             (str1, VFonctionC f) -> case interpreteC env e2 of
                                          (str2, val) -> (str1 ++ str2 ++ "." ++ fst (f val), snd (f val))
             (_, _)               -> error (show e1 ++ " n'est pas une fonction, application impossible")


-- Q27.
pingC :: ValeurC
pingC = VFonctionC (\v -> ("p", v))


-- -- -- -- -- -- -- -- --
-- Interprète monadique --
-- -- -- -- -- -- -- -- --

data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))


-- Q28.
instance Show (ValeurM m) where
        show (VLitteralM (Entier n)) = show n
        show (VLitteralM (Bool b))   = show b
        show (VFonctionM _)          = "λ"

data SimpleM v = S v
               deriving Show


-- Q29.

-- getSimpleM :: SimpleM m -> m
-- getSimpleM (S v) = v

interpreteSimpleM :: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)
interpreteSimpleM _   (Lit l)     = S (VLitteralM l)
interpreteSimpleM env (Var x)     = S (fromJust (lookup x env))
interpreteSimpleM env (Lam x e)   = S (VFonctionM (\v -> interpreteSimpleM ((x, v):env) e))
interpreteSimpleM env (App e1 e2) = vfonction (interpreteSimpleM env e1) (getSimpleM (interpreteSimpleM env e2))
                                    where vfonction (S (VFonctionM f)) = f
                                          vfonction _                  = undefined
                                          getSimpleM (S v)             = v


-- Q30.
fmap  :: (a -> b) -> SimpleM a -> SimpleM b
fmap f (S a) = pure (f a)

(<*>) :: SimpleM (a -> b) -> SimpleM a -> SimpleM b
(<*>) (S f) (S a) = pure (f a)

instance Functor SimpleM where
    fmap = liftM

instance Applicative SimpleM where
    pure  = S
    (<*>) = ap

instance Monad SimpleM where
    (S v) >>= f = f v


-- Q31.
interpreteM :: Monad m => Environnement (ValeurM m) -> Expression -> m (ValeurM m)
interpreteM _   (Lit l)     = pure (VLitteralM l)
interpreteM env (Var x)     = pure (fromJust (lookup x env))
interpreteM env (Lam x e)   = pure (VFonctionM (\v -> interpreteM ((x, v):env) e))
interpreteM env (App e1 e2) = (interpreteM env e1) >>=
        (\app -> case app of
                      (VFonctionM f) -> (interpreteM env e2) >>= (\v -> f v)
                      _              -> error (show app ++ " n'est pas une fonction, application impossible"))


-- Q32.
-- Il est vérifé dans `Test.hs : test32` que interpreteS se comporte comme interpreteSimpleM.

type InterpreteM m = Environnement (ValeurM m) -> Expression -> m (ValeurM m)

interpreteS :: InterpreteM SimpleM
interpreteS = interpreteM


-- -- -- -- -- -- -- -- --
--   Variante traçante  --
-- -- -- -- -- -- -- -- --

data TraceM v = T (Trace, v)
              deriving Show

-- Q33.
instance Functor TraceM where
        fmap = liftM

instance Applicative TraceM where
        pure v = T ("", v)
        (<*>) = ap

instance Monad TraceM where
        T (t, v) >>= f = case f v of
                              T (t2, v2) -> T (t ++ t2, v2)

interpreteMT :: InterpreteM TraceM
interpreteMT = interpreteM

pingM :: ValeurM TraceM
pingM = VFonctionM (\v -> T ("p", v))


-- Q34
interpreteMT' :: InterpreteM TraceM
interpreteMT' env (Lam x e)   = pure (VFonctionM (\v -> interpreteMT' ((x, v):env) e)) >>= (\v -> T ("",v) )
interpreteMT' env (App e1 e2) = (interpreteMT' env e1) >>=
                                \v1 -> T (".", v1) >>=
                                \v2 -> case v2 of
                                           (VFonctionM f) -> interpreteMT' env e2 >>=
                                                             \v3 -> f v3
                                           _              -> error (show v2 ++ " n'est pas une fonction, application impossible")
interpreteMT' env x = interpreteM env x


-- -- -- -- -- -- -- -- -- --
--  Variante avec erreurs  --
-- -- -- -- -- -- -- -- -- --
data ErreurM v = Succes v
               | Erreur String
               deriving Show

-- Q35
instance Functor ErreurM where
        fmap = liftM

instance Applicative ErreurM where
        pure = Succes
        (<*>) = ap

instance Monad ErreurM where
        fail e = Erreur e
        (Succes v) >>= f = f v
        (Erreur str) >>= _ = fail str


-- Q36
interpreteE :: Monad m => InterpreteM m
interpreteE _ (Lit l)       = pure $ VLitteralM l
interpreteE env (Var x)     = case lookup x env of
                                   Just v  -> pure v
                                   Nothing -> fail ("la variable " ++ x ++ " n'est pas definie")
interpreteE env (Lam x e)   = pure $ VFonctionM (\v -> interpreteE ((x, v):env) e)
interpreteE env (App e1 e2) = (interpreteE env e1) >>=
        \v1 -> case v1 of
                    (VFonctionM f) -> (interpreteE env e2) >>= (\v2 -> (f v2)) >>= \v3 -> pure v3
                    _              -> fail (show v1 ++ " n'est pas une fonction, application impossible")

type InterpreteE m = InterpreteM m

interpreteEE :: InterpreteE ErreurM
interpreteEE = interpreteE


-- -- -- -- -- -- -- -- -- -- --
--    Cerise sur le gateau    --
-- -- -- -- -- -- -- -- -- -- --

-- Q37.
class Injectable m t where
        injecte :: t -> ValeurM m

instance Injectable m Bool where
        injecte = VLitteralM . Bool

instance Injectable m Integer where
        injecte = VLitteralM . Entier


-- Q38.
instance (Monad m, Injectable m t) => Injectable m (Bool -> t) where
        injecte f = VFonctionM (\(VLitteralM (Bool b)) -> return (injecte (f b)))

instance (Monad m, Injectable m t) => Injectable m (Integer -> t) where
        injecte f = VFonctionM (\(VLitteralM (Entier x)) -> return (injecte (f x)))


-- Q39. + Q41.
envM :: Monad m => Environnement (ValeurM m)
envM = [ ("add",   injecte ((+) :: Integer -> Integer -> Integer))
       , ("soust", injecte ((-) :: Integer -> Integer -> Integer))
       , ("mult",  injecte ((*) :: Integer -> Integer -> Integer))
       , ("quot",  injecte (quot :: Integer -> Integer -> Integer))
       , ("et",    injecte (&&))
       , ("ou",    injecte (||))
       , ("non",   injecte not)
       , ("if",    injecte (ifM :: Bool -> Integer -> Integer -> Integer))
       , ("infst", injecte ((<) :: Integer -> Integer -> Bool))]


-- Interprete monadique Interactive
interpreter :: IO ()
interpreter = do
                putStr "minilang > "
                hFlush stdout
                str <- getLine
                let res = interpreteEE envM (ras str)
                putStrLn (show res)
                interpreter
                fin <- isEOF
                unless fin interpreter


-- -- -- -- -- -- -- --
--    Pousse-cafe    --
-- -- -- -- -- -- -- --

-- Q40

-- Les `\lazy -> ...` permettent de forcer l'évaluation des expressions qui ne seront pas évaluées normalement que si Haskell en avait besoin.


-- Q41
ifM :: Bool -> a -> a -> a
ifM b x y = case b of
                 True  -> x
                 False -> y

-- ifM' :: ValeurM m
-- ifM' = VFonctionM(\ cond -> case cond of
--         (VLitteralM (Entier n))  -> fail (show cond ++ " n'est pas une condition, application impossible")
--         (VLitteralM (Bool b))    -> case b of
--                 True  -> (VFonctionM (\a -> (VFonctionM (\_ -> a))))
--                 False -> (VFonctionM (\_ -> (VFonctionM (\c -> c))))
--         (VFonctionM f)           -> f >>= (\ b -> case b of
--                 True  -> (VFonctionM (\a -> (VFonctionM (\_ -> a))))
--                 False -> (VFonctionM (\_ -> (VFonctionM (\c -> c))))
-- --                 ))

-- -- ifM' :: ValeurM m
-- ifM' :: ValeurM m -> (ValeurM m -> Bool) -> ValeurM m -> ValeurM m -> b
-- ifM' cond x y = case cond of
--         (VLitteralM (Entier n))  -> fail (show cond ++ " n'est pas une condition, application impossible")
--         (VLitteralM (Bool b))    -> case b of
--                 True  -> x
--                 False -> y
--         (VFonctionM f)           -> f >>= \ b -> case b of
--                 True  -> x
--                 False -> y



-- Q42