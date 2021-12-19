#   Premier et deuxième contacts

## Travail TP PF L3S6 Informatique

## Jayjaywantee KOODUN et Thi-Ngoc-Anh TRAN

Ce dépôt correspond aux deux premières séances de TP de PF.

## 1. Contenu du dépôt

Il contient les fichiers suivants :

-   `answersTp1.hs`, `answersTp2.hs` contient des fonctions créées en TP1 et TP2,
-   `testTp1.txt`, `testTp2.txt` contient des tests pour les fonctions dans answersTp1.hs et answersTp2.hs,

-   `dragon.hs` est le canevas pour le dessin de la courbe du Dragon,
-   `dragon2.hs` est le canevas alternative pour le dessin de la courbe du Dragon,

-   `Setup.hs`, `premiers-contacts.cabal`, `stack.yaml` sont des
    fichiers de configuration pour
    [Cabal](https://www.haskell.org/cabal/) et
    [Stack](https://docs.haskellstack.org/en/stable/README/).

Ces fichiers ne sont réellement utiles que si vous voulez utiliser ces
outils pour installer les bibliothèques nécessaires sur une machine
personnelle.

## 2. Exécuter Haskell
Lancer Haskell

> ghci

Charger/load le fichier source 

```
Prelude> :l answersTp1.hs
  *Main> :l answersTp2.hs
  *Main> :l dragon.hs
  *Main> :l dragon2.hs
```

Compiler et exécuter le fichier GHCi

> make all

Executer la courbe dragon (1ere version)

> ./dragon

Executer la courbe dragon (version alternative)

> ./dragon2

Nettoyer le repertoire

> make clean

## 3. Utilisation de `stack` sur votre machine personnelle.

Cette section n’est pas un substitut à la documentation de l’outil.
Elle vous donne juste une commande qui peut aider à démarrer.

Si vous voulez installer la bibliothèque Gloss nécessaire pour
dessiner la courbe du Dragon en utilisant l’outil `stack` (que vous
devrez avoir installé, bien entendu), vous pourrez utiliser :

```console
stack --install-ghc build
```

Cette commande devrait éventuellement installer GHC, Gloss, et autres
dépendances puis compiler `dragon.hs` en un exécutable créé quelque
part dans le répertoire `.stack-work/install` (le chemin dépend de
votre configuration).