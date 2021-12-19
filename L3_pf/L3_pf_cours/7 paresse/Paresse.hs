module Paresse where

-- Ordre d’évaluation
-- Paresse est un abus de langage pour dire que Haskell est
-- un langage à évaluation non-stricte

import System.Environment

carre n = n * n

ex1 = carre (1+2)

-- Deux évaluations possibles
--
-- Évaluation stricte, ou appel par valeur (call by value)
-- Évaluation des arguments avant de faire un appel de fonction
-- carre (1+2) = carre 3
--             = 3 * 3
--             = 9
--
-- Évaluation non-stricte, en l’occurrence appel par nom (call by name)
-- carre (1+2) = (1+2) * (1+2)
--             = 3 * (1+2)
--             = 3 * 3
--             = 9
--
-- 9 est dit _forme normale_, une expression qu’on ne peut plus simplifier
--
-- Toujours le même résultat, quel que soit l’ordre
-- d’évaluation dans du code pur


trouNoir = trouNoir

ex2 = trouNoir

ex3 = fst (1, trouNoir)
ex4 = snd (1, trouNoir)

ex5 = fst (1, undefined)
ex6 = snd (1, undefined)


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ e = e

ex7  = ifThenElse True  undefined 12
ex8  = ifThenElse False undefined 12
ex9  = ifThenElse True  12 undefined
ex10 = ifThenElse False 12 undefined

(&&&) :: Bool -> Bool -> Bool
_ &&& False = False
b &&& True  = b

ex11 = undefined &&& True
ex12 = undefined &&& False
ex13 = True &&& undefined
ex14 = False &&& undefined

ex15 = undefined && True
ex16 = undefined && False
ex17 = True && undefined
ex18 = False && undefined


-- Évaluation dite paresseuse, ou appel par nécessité (call by need)
-- Fermeture = code + environnement
-- let ... in est un peu similaire à un environnement
--
-- carre (1+2) = let n = 1+2 in n * n
--             = let n = 3   in n * n
--             = 9

boum = [1, 2, undefined]
boum' = [1, 2, undefined, 4]

long = [0..100000]
quatre = long !! 4

uns = 1 : uns
uns' = repeat 1
uns'' = iterate id 1

fibo :: [Integer]
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)

-- Construit un exécutable calculant le n^e nombre de Fibonacci : montre que GHC
-- crée un exécutable utilisant très peu de mémoire (il se rend compte que les
-- valeurs intermédiaires ne sont pas nécessaires dès que le nombre de Fibonacci
-- suivant est calculé), alors que l’évaluation dans GHCi mémorise tous les
-- éléments de la liste qui ont été calculés
-- À comparer donc avec GHCi, en tapant :set +s et en regardant la
-- mémoire allouée par GHCi après le calcul de fib !! 100000 (si vous
-- avez assez de mémoire sur votre machine ! Il prend 1,5Go chez moi)
mainFibo :: IO ()
mainFibo = do (n: _) <- getArgs
              print (fibo !! read n)

plus1 = map (1+) [0..]

douze = let _ = plus1 !! 20
        in  12

douze' = (plus1 !! 20) `seq` 12


-- Gain en modularité
controle = take 50
donnees  = [1..]

programme = controle donnees

premiers :: [Integer]
premiers = crible [2..]
    where crible (p:ns) = p : crible (filter (\n -> n `mod` p /= 0) ns)

-- avec du contrôle
ex19 = premiers !! 4000
ex20 = take 10 premiers
ex21 = takeWhile (< 1000) premiers


-- Entrées / sorties paresseuses
mainIO :: IO ()
mainIO = do contenu <- readFile "/tmp/test"
            writeFile "/tmp/test" (reverse contenu)

mainIO2 :: IO ()
mainIO2 = do contenu <- readFile "/tmp/test"
             putStrLn "tapez entrée pour continuer (vous pouvez en profiter pour changer le contenu de /tmp/test)"
             getChar
             putStrLn contenu
