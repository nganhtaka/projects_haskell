# ACT - Algorithmes et Complexité

## Rapport TP1 - Diviser pour régner. Le plus grand rectangle.

## Jayjaywantee KOODUN et Thi-Ngoc-Anh TRAN
**Groupe 2** M1S1 2018/2019


###  Contenu du dépôt
- `Main.hs`
- `Setup.hs`

###  Comment exécuter :

Le fichier `Main.hs` contient une fonction `main` qui permet de calculer la plus grande surface (`maxArea`) selon l'algorithme qu'on choisit. Il suffit de décommenter la ligne correspondante à l'algorithme qu'on choisit et de commenter les 3 autres lignes :

- ligne 12 -- Algorithme en O(n^3) : `grandCarreO3`
- ligne 13 -- Algorithme en O(n^2) : `grandCarreO2`
- ligne 14 -- Diviser pour regner  : `grandCarreRegner`
- ligne 15 -- Algorithme en O(n)   : `grandCarreOn`

Pour compiler et produire l'éxécutable :
```
$ make
```

Pour tester sur les données :

```
$ ./Main < verifDonneesTP1/ex_N10_res24400144
```

---

### Sommaire :
- Q1. Une première approche
- Q2. Diviser pour régner
- Explication des tests

#### Q1. Un première approche

La surface du rectangle de surface maximal est construit par une abscisse gauche, une abscisse droite et un point au milieu de 2 abscisses telque son y est minimum parmi tous les points entre 2 abscisses.

Suivant ce sont les 2 algorithme en O(n^3) et O(n^2). L'algorithme O(n^3) ne passe pas tous les tests de 1 à 8. Et le test 8, il prend 0.148s pour finir sa fonction. Alors que l'algorithme O(n^2) passe tous les tests de 1 à 8 et juste 0.012s pour le test 8.

Codechef demande l'algorithme s'exécute sur leur plateforme en moins de 1s (1ms = 10^-6 s par exemple) pour n=100000.
Donc avec O(n^2) ça prend (10^-6 * (10^5)^2) = 10000 ~ 3h.
Avec O(n^3) ça prend (10^-6 * (10^5)^3) = 10^9 ~ 32 ans.

1. `Algorithme de O(n^3)`
    ```
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
    ```

**Analyse de l'algorithme** :
- Troisième boucle parcourir tous les points entre 2 axis pour trouver le point a son y_min donc = O(n)
- Deuxième boucle parcourir les axis droites = O(n), dont calculer et comparer le surface avec y_min trouvé.
- Première boucle parcourir les axis gauches = O(n)
- Donc O(n * n * n) = O(n^3)


2. `Algorithme de O(n^2)`
    ```
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
    ```

**Analyse de l'algorithme** :
- Deuxième boucle parcourir les axis droites = O(n), dont
    + comparer le y_min avec le y du point plus proche de gauche
    + calculer et comparer le surface avec y_min trouvé.
- Première boucle parcourir les axis gauches = O(n)
- Donc O(n*n) = O(n^2)


#### Q2. Diviser pour régner

1. `Algorithme : diviser pour régner`

    ```
    Chercher le point p avec son y plus petit
    Premier point est le premier point de la liste (son x est plus petit)
    Dernier point est le dernier point de la liste (son x est plus grand)
    Diviser la liste des points en 2 selon ce point p
        Calculer la surface1 : surface maximum de la zone à la gauche de cette point p

        Calculer la surface2 de la zone entre x(premier point), x(dernier point) et y

        Calculer la surface3 : surface maximum de la zone a la droite de cette point p

        Retourner la surface maximum parmi [surface1, surface2, surface3]
    ```

Algorithme "diviser pour régner" est réalisable pour ce cas.


**Analyse de l'algorithme** :

- Chercher le point p avec son y plus petit     = O(n)
- Diviser la liste des points en 2              = O(n) pour le pire des cas et O(1) pour le meilleur des cas
- Calculer la surface à la gauche du point p    = O(n/2)
- Calculer la surface contient le premier point, dernier point et le point p = O(1)
- Calculer la surface à la droit du point p     = O(n/2)
- Comparer pour trouver la surface maximal
- Donc 0(3n log n) = 0 (n log n)

Vérification par le Master Théorème :

    T(n) = 2 * T([n/2]) + O(n)
    d=1 = log(2)2 = 1 donc O(n log n)

L'algorithme O(n^3) ne passe pas tous les tests de 1 à 9. Il prend 0.004s pour le test 8 et 0.712s pour le test 9.


#### Explication des tests

__Test sur plateforme : copier/coller le code de Main.hs.__

__Test sur repertoire TP1_ACT (contient Makefile)__ :

    + Exemple : pour tester le O(n^3) decommenter ligne 12 et commenter les autres lignes maxArea
    + Exemple : pour tester le O(n^2) decommenter ligne 13 et commenter les autres lignes maxArea

    ```
    $ make

    $ ./Main < verifDonneesTP1/ex_N10_res24400144
    24400144
    ```

__Test sur ghci__
    ```
    $ ghci

    > :l Main.hs
    > grandCarreO3 (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]
    153
    > grandCarreO2 (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]
    153
    > grandCarreRegner (25,20) 5 [(2,5),(5,17),(11,4),(16,6),(20,1)]
    153
    ```