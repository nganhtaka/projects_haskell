module Tortue where

------------- Tortue -----------
import Graphics.Gloss
import TortueImplementation

type EtatTortue = (Point, Float)

-- Tortue volante
type EtatDessin = ([EtatTortue], [Path])


type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue

-- Q4.

etatInitial :: Config -> EtatTortue
etatInitial (a, _, _, _, _) = a

longueurPas :: Config -> Float
longueurPas (_, a, _, _, _) = a

facteurEchelle :: Config -> Float
facteurEchelle (_, _, a, _, _) = a

angle :: Config -> Float
angle (_, _, _, a, _) = a

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_, _, _, _, a) = a

-- Q5.

avance :: Config -> EtatTortue -> EtatTortue
avance config ((x, y), cap) = ((x1, y1), cap)
    where x1 = x + (longueurPas config) * (cos cap)
          y1 = y + (longueurPas config) * (sin cap)

-- Q6.

tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche config (point, cap) = (point, cap + angle config)

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite config (point, cap) = (point, cap - angle config)

-- Q7.

filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue config xs = filter (`elem` symbolesTortue config) xs

-- filtreSymbolesTortue' :: Config -> Mot -> Mot
-- filtreSymbolesTortue' config xs = [x | x <- xs, x `elem` symbolesTortue config]


-- Q8. / Q12. / Q13.

interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config (etatCurrent:etat, pathCurrent:path) sym =
    case sym of
        'F' -> (e:etat, ([fst e] ++ pathCurrent):path) where e = avance config etatCurrent
        '+' -> (e:etat, pathCurrent:path) where e = tourneAGauche config etatCurrent
        '-' -> (e:etat, pathCurrent:path) where e = tourneADroite config etatCurrent
        '[' -> (etatCurrent:etatCurrent:etat, pathCurrent:pathCurrent:path)    -- save current state and position
        ']' -> (etat, pathCurrent:path)             -- replace current state by last state saved
        _   -> (etatCurrent:etat, pathCurrent:path)

-- Q9.
{-
Dans la fonction interpreteSymbole, nous avons ajouté en tête le nouveau point dans le chemin.
Pour éviter de parcourir toute la liste des points, dans le cas où on ajoute en queue.
-}


-- Q10.
transformerEtat :: Config -> EtatDessin -> Mot -> EtatDessin
transformerEtat config etat xs = foldl (interpreteSymbole config) etat xs

-- version recursive
transformerEtat' :: Config -> EtatDessin -> Mot -> EtatDessin
transformerEtat' _ etat [] = etat
transformerEtat' config etat (x:xs) = transformerEtat config (interpreteSymbole config etat x) xs


interpreteMot :: Config -> Mot -> Picture
interpreteMot config mot = let m         = filtreSymbolesTortue config mot
                               etatInit  = [etatInitial config]
                               pointInit = [fst (etatInitial config)] in
                               line . head $ snd $ transformerEtat config (etatInit, [pointInit]) m


-- Creation du dessin Flocon de von Koch, au tout début
dessin :: Picture
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

-- main :: IO ()
-- main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin


-- Q11.

lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime ls (e, l_init, facteur, a, s) instant = interpreteMot config' (ls !! enieme)
                                where config' = (e, l_init * (facteur ^ enieme), facteur, a, s)
                                      enieme  = (round instant `mod` 8)

-- Exemples des courbes

vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")


-- Exemples botanistes

brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]

brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")


-- main animation
{- main :: IO ()
main = animate (InWindow "L-système" (1000, 1000) (0, 0)) white brindilleAnime
 -}