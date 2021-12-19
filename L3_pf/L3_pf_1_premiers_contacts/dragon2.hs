module Main where

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (-100,100) (300,100))

dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))

-- Q5. 
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa,ya) (xb,yb) = ((xa+xb)/2 + (yb-ya)/2, (ya+yb)/2 + (xa-xb)/2)

-- Q8.
dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre a b 0 = [a,b]
dragonOrdre a b n|n>0 = dragonOrdre a c (n-1) ++ reverse (dragonOrdre b c (n-1))
            where   c = pointAintercaler a b

{-
Si on écrit : 
    dragonOrdre :: Point -> Point -> Int -> Path
    dragonOrdre a b 0 = [a,b]
    dragonOrdre a b n|n>0 = dragonOrdre a c (n-1) ++ dragonOrdre b c (n-1)
                where   c = pointAintercaler a b

On a le résultat : 
    a b 0 = a,b 
    a b 1 = ac0 ++ bc0 = [a,c,b,c],  avec c est pointAintercaler de ab
    a b 2 = ac1 ++ bc1 = ad0++cd0 ++ be0++ce0 = [a,d,c,d,b,e,c,e], 
        avec d est pointAintercaler de ac, e est pointAintercaler de bc
        on voit que entre d et b (3ème et 4ème point), il y a un trait (line) en trop, 
        car d et b ne sont pas à côté : a-d-c-d-b-e-c-e
        Vaut mieux l'inverser 2ème dragonOrdre pour avoir [a,d,d,c,c,e,e,b] : a-d-c-e-b

Faut écrire : 
    dragonOrdre a b n|n>0 = dragonOrdre a c (n-1) ++ reverse (dragonOrdre b c (n-1))
                where   c = pointAintercaler a b

Donc le résultat : 
    a b 0 = a,b 
    a b 1 = ac0 ++ reverse (bc0) = [a,c,c,b]
    a b 2 = ac1 ++ reverse (bc1) = ad0   ++ reverse (cd0) ++ reverse (be0++ reverse (ce0)) = 
                                 = [a,d] ++    [d,c]      ++ reverse [b,e] ++ [e,c]), 
                                 = [a,d,d,c,c,e,e,b]    -> pas de trait en trop
-}

-- Q9.
{-
La courbe dragon au temps (t `mod` 20) est l'ordre de cette courbe
Donc : 
    dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))
-}