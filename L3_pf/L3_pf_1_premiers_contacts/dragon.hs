module Main where

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (-100,100) (300,100))

dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (dragon a b !! (round t `mod` 20))

-- Q5. 
pointAintercaler :: Point -> Point -> Point
pointAintercaler (xa,ya) (xb,yb) = ((xa+xb)/2 + (yb-ya)/2, (ya+yb)/2 + (xa-xb)/2)

-- Q6.
pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [a] = [a]
pasDragon [a,b] = [a, pointAintercaler a b, b]
pasDragon (a:b:c:xs) = a : pointAintercaler a b : b : pointAintercaler c b : pasDragon (c:xs)


-- [[a,b], [a,(ab),b], [a,(a ab),(ab),(ab b),b]]

-- Q7.
dragon :: Point -> Point -> [Path]
dragon a b = iterate pasDragon (pasDragon [a,b])