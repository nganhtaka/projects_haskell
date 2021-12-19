module Main where

import Tortue
import Graphics.Gloss
import TortueImplementation

main :: IO ()
main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white brindilleAnime
