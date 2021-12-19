module Main where

    type Point = (Int, Int)
    
    main :: IO()
    main = do
        contents <- getContents
        let (dimensions:num:rest)  = lines contents
            [l, h]                 = read <$> words dimensions
            size                   = read num
            points                 = map parseCoords (take size rest)
            --maxArea                = grandCarreO3 (l,h) size points                         -- Algorithme en O(n^3)
            --maxArea                = grandCarreO2 (l,h) size points                         -- Algorithme en O(n^2)
            maxArea                = grandCarreRegner (l,h) size points                     -- Diviser pour regner
            --maxArea                = grandCarreOn (l,h) size points                         -- Algorithme en O(n)
        putStrLn. show $ maxArea
        where
            parseCoords (coord) = let [a, b] = read <$> words coord
                                    in (a, b)
    
    
    -- Calculer la surface avec Int:xG, Int:xD, Int:y, Int:surface
    calculSurface :: Int -> Int -> Int -> Int
    calculSurface xG xD y = (xD-xG)*y
    
    -- -- -- -- -- -- -- -- -- -- --
    --  Q1. Algorithme en O(nˆ3)  --
    -- -- -- -- -- -- -- -- -- -- --
    grandCarreO3 :: Point -> Int -> [Point] -> Int
    grandCarreO3 (0,_) _ _       = 0
    grandCarreO3 (_,0) _ _       = 0
    grandCarreO3 (l,h) 0 _       = calculSurface 0 l h
    grandCarreO3 (l,_) 1 [p]     = calculSurface 0 l (snd p) 
    grandCarreO3 (_,h) 2 [p1,p2] = calculSurface (fst p1) (fst p2) h
    grandCarreO3 (l,h) n ps      = chercherAvecAxisGauche h (n+2) 0 ([(0,0)] ++ ps ++ [(l,h)]) calculSurface
    
    chercherAvecAxisGauche :: Int -> Int -> Int -> [Point] -> (Int->Int->Int->Int) -> Int
    chercherAvecAxisGauche _ 0 _ _ _          = 0
    chercherAvecAxisGauche h n i ps f | n <=i = 0
                                      | otherwise = let xG       = fst (ps !! i)
                                                        liste    = drop (i+1) ps
                                                        fistElem = chercherAvecAxisDroit h (length liste) xG 0 liste f
                                                        lastElem = (chercherAvecAxisGauche h (n-1) (i+1) ps f) 
                                                    in  maximum [fistElem,lastElem]
    
    chercherAvecAxisDroit :: Int -> Int -> Int -> Int -> [Point] -> (Int->Int->Int->Int) -> Int
    chercherAvecAxisDroit _ 0 _ _ _ _          = 0
    chercherAvecAxisDroit h 1 xG _ [p] f       = f xG (fst p) h
    chercherAvecAxisDroit h n xG j ps f | n<=j = 0
                                        | otherwise = let xD       = fst (ps !! j)
                                                          liste    = take j ps
                                                          fistElem = chercherYMin h (length liste) xG xD liste f
                                                          lastElem = (chercherAvecAxisDroit h (n-1) xG (j+1) ps f) 
                                                      in maximum [fistElem,lastElem]
    
    chercherYMin :: Int -> Int -> Int -> Int -> [Point] -> (Int->Int->Int->Int) -> Int
    chercherYMin h 0 xG xD _ f  = f xG xD h
    chercherYMin h _ xG xD ps f = f xG xD (minimum (h:(map snd ps)))
    
    
    -- -- -- -- -- -- -- -- -- -- --
    --  Q1. Algorithme en O(nˆ2)  --
    -- -- -- -- -- -- -- -- -- -- --
    grandCarreO2 :: Point -> Int -> [Point] -> Int
    grandCarreO2 (0,_) _ _       = 0
    grandCarreO2 (_,0) _ _       = 0
    grandCarreO2 (l,h) 0 _       = calculSurface 0 l h
    grandCarreO2 (l,_) 1 [p]     = calculSurface 0 l (snd p) 
    grandCarreO2 (_,h) 2 [p1,p2] = calculSurface (fst p1) (fst p2) h
    grandCarreO2 (l,h) n ps      = chercherAvecAxisGauche2 h (n+2) ([(0,0)] ++ ps ++ [(l,h)]) calculSurface
    
    chercherAvecAxisGauche2 :: Int -> Int -> [Point] -> (Int->Int->Int->Int) -> Int
    chercherAvecAxisGauche2 _ 0 _ _      = 0
    chercherAvecAxisGauche2 h n (p:ps) f = let xG       = fst p
                                               fistElem = chercherAvecAxisDroit2 (n-1) xG h ps f
                                               lastElem = (chercherAvecAxisGauche2 h (n-1) ps f) 
                                           in maximum [fistElem,lastElem]
    
    chercherAvecAxisDroit2 :: Int -> Int -> Int -> [Point] -> (Int->Int->Int->Int) -> Int
    chercherAvecAxisDroit2 0 _ _ _ _               = 0
    chercherAvecAxisDroit2 1 xG min_y [p] f        = f xG (fst p) min_y
    chercherAvecAxisDroit2 n xG min_y (p1:p2:ps) f = let xD       = fst p2
                                                         y        = minimum [snd p1, min_y]
                                                         fistElem = f xG xD y
                                                         lastElem = (chercherAvecAxisDroit2 (n-1) xG y (p2:ps) f) 
                                                     in maximum [fistElem, lastElem]
    
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    --        Q2. Algorithme : Diviser pour régner           --
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    chercherIndexYMin :: Int -> Int -> Int -> Int -> [Point] -> ((Int,Int),Int)
    chercherIndexYMin indice _ x y_min []                              = ((x,y_min),indice)
    chercherIndexYMin indice indice_p x y_min (p:ps) | (snd p < y_min) = chercherIndexYMin indice_p (indice_p+1) (fst p) (snd p) ps
                                                     | otherwise       = chercherIndexYMin indice (indice_p+1) x y_min ps
    
    grandCarreRegner :: Point -> Int -> [Point] -> Int
    grandCarreRegner (0,_) _ _   = 0
    grandCarreRegner (_,0) _ _   = 0
    grandCarreRegner (l,h) 0 _   = calculSurface 0 l h
    grandCarreRegner (l,_) 1 [p] = calculSurface 0 l (snd p) 
    grandCarreRegner (l,h) n ps  = maxSurfaceRegner h n 0 l ps calculSurface
    
    maxSurfaceRegner :: Int -> Int -> Int -> Int -> [Point] -> (Int->Int->Int->Int)-> Int
    maxSurfaceRegner h 0 xG xD _ f      = f xG xD h
    
    maxSurfaceRegner h n xG xD (p:ps) f = 
        let ((x_lowestPoint, y_lowestPoint), index_lowestPoint) = chercherIndexYMin 0 1 (fst p) (snd p) ps
            zoneYMin            = f xG xD y_lowestPoint
            (liste1,(_:liste2)) = splitAt index_lowestPoint (p:ps)
            zoneGauche          = maxSurfaceRegner h index_lowestPoint xG x_lowestPoint liste1 f
            zoneDroit           = maxSurfaceRegner h (n- index_lowestPoint-1) x_lowestPoint xD liste2 f 
        in maximum [zoneGauche, zoneYMin, zoneDroit]
    
    
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    --         Q4. Algorithme en O(n)                        --
    -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
    
    grandCarreOn :: Point -> Int -> [Point] -> Int
    grandCarreOn (0,_) _ _       = 0
    grandCarreOn (_,0) _ _       = 0
    grandCarreOn (l,h) 0 _       = calculSurface 0 l h
    grandCarreOn (l,_) 1 [p]     = calculSurface 0 l (snd p) 
    grandCarreOn (_,h) 2 [p1,p2] = calculSurface (fst p1) (fst p2) h
    grandCarreOn (l,h) n (p:ps)  = maxSurfaceOn h (n-1) ((0,0),p) (snd p) ps ((fst p)*h) calculSurface
    
    maxSurfaceOn :: Int -> Int -> (Point, Point) -> Int -> [Point] -> Int -> (Int->Int->Int->Int) -> Int
    maxSurfaceOn _ 0 _ _ _ max_area _ = max_area
    
    maxSurfaceOn h 1 (pointGauche,pointMiddle) y_min [pointDroit] max_area f = 
        let surface1 = f (fst pointMiddle) (fst pointDroit) h
            y        = minimum [snd pointMiddle, y_min]
            surface2 = f (fst pointGauche) (fst pointDroit) y
        in maximum [surface1, surface2, max_area]
    
    maxSurfaceOn h n (pointGauche,pointMiddle) y_min (pointDroit:ps) max_area f = 
        let y        = minimum [snd pointMiddle, y_min]
            surface1 = f (fst pointGauche) (fst pointDroit) y
            surface2 = f (fst pointMiddle) (fst pointDroit) h
            surface  = maximum [surface1, surface2, max_area]
        in case (surface1 <= surface2) of
           _ | True      -> maxSurfaceOn h (n-1) (pointGauche, pointDroit) y ps surface f
             | otherwise -> maxSurfaceOn h (n-1) (pointMiddle, pointDroit) (snd pointMiddle) ps surface f