#   Interprétation

Ce dépôt correspond au TP de PF « Interprétation ».

##  Jayjaywantee KOODUN et Thi-Ngoc-Anh TRAN
##  L3S6 2017/2018

## Ce qui a été fait :
On a fait tous les questions de ce TP.

##  Contenu du dépôt
- `Parser.hs`
- `Interprete.hs`
- `Test.hs`
- `Main.hs`


## Ce qui a été fait
On a traité tous les questions demandées jusqu'au question 41.

## Ce qui n'a pas été fait
On n'arrive pas à tester *fibonacci.interp* avec notre interprète. En effet, notre implementation de la primitive *if* dans l'interprète monadique n'est pas bien faite et cela ne nous permet pas de tester *fibonacci.interp*.

## Compiler et exécuter

Pour compiler depuis le terminal :
> $ make main

Pour exécuter l'interprète interactive, lancez la commande suivant :

> $ rlwrap ./Main

```
minilang > add 1 3
4
minilang > add 3 4
7
```

Pour lancer l'interprète sur le *fibonacci.interp*
> $ ./Main < fibonacci.interp

## Tester

Pour tester, lancer d'abord `ghci` depuis le terminal, et ensuite :

```
Prelude> :load Test.hs
*Test> runTestTT testAll
```