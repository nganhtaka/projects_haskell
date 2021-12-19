module Mystere where

import Data.List (sort)
import Test.QuickCheck

myst     [] = []
myst (x:xs) = myst ys ++ [x] ++ myst zs
    where ys = [ y | y <- xs, y <= x]
          zs = [ z | z <- xs, z > x]

prop_myst xs = myst xs == sort xs

-- dans GHCi, lancer :
-- quickCheck prop_myst
