
Ce dépôt correspond au TP1 de ACT "Le plus grand rectangle". La réponse des questions du TP et le test pour tester les données.

## Jayjaywantee KOODUN et Thi-Ngoc-Anh TRAN

### M1S1 2018/2019


## Note : Test sur terminal

- Test sur plateforme : copier/coller le code de Main.hs.

- Test sur repertoire TP1_ACT (contient Makefile) :
    + Exemple : pour tester le O(n^3) decommenter ligne 15 et commenter les autres lignes maxArea
    + Exemple : pour tester le O(n^2) decommenter ligne 16 et commenter les autres lignes maxArea
    + test est une fichier contient les donnees de l'exemple du sujet TP1
```
$ make

$ ./Main < test
153
```

- Test sur ghci
```
$ ghci

> :l Main.hs

> grandCarreO3 (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]
153

> grandCarreO2 (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]
153

> maxAreaDivideRule 25 20 7 [(0,0),(2,5),(5,17),(11,4),(16,6),(20,1),(0,25)]
153
```

- Sujet
    + l : length, h : height
    + n : nb de points
    + list des 5 points (x,y) trié selon l'ordre de x
    + $ ./a.out < fichier_entree
    + $ cat /proc/cpuinfo

## Q1. Une première approche

#### `Algorithme en O(nˆ3) :`
    int surface_max = 0; int min_y=h;
    pour tous les axis (gauches) :
        min_y=h
        pour tous les axis (droits) :
            Pour tous les points entre les 2 axis (contient pas les points de 2 axis)
                Trouver min_y
            fin pour
            Calculer le surface avec min_y
            si surface > surface_max
                surface_max = surface;
        fin pour
    fin pour
    return surface_max

- Exemple : grandCarreO3 (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]


#### `Algorithme en O(n^2)`
    int surface_max = 0; int min_y=h;
    pour tous les axis (gauches) :
        min_y=h
        pour tous les axis (droits) :
            Avec le point plus proche à gauche (contient pas les points de 2 axis)
                si (son y < y_min)
                    min_y =y
            Calculer le surface avec : min_y
            si surface > surface_max
                surface_max = surface;
        fin pour
    fin pour
    return surface_max
    
- Exemple : grandCarreO2 (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]

## Q2. Diviser pour régner
#### `Algorithme : diviser pour régner`
    Diviser la liste des points en 2 selon le point p telque y(p) est plus petit parmi la liste des points
        Calculer la surface1 : surface maximum de la zone a la gauche de cette point p
        Calculer la surface2 de la zone entre x(premier point), x(dernier point) et y(p)
        Calculer la surface3 : surface maximum de la zone a la droite de cette point p
        Retourner la surface maximum parmi [surface1, surface2, surface3]

    (split list into half based on the smallest y => (ymin)
    that gives you the greatest possible area
    recursively do the same in the 2 partitions of the remaining list
    in the end take the max of the areas that you get from both lists + area of initial call to the function)
