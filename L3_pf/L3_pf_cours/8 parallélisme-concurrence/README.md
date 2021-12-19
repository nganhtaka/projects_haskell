#   Parallélisme et Concurrence

Référence : le livre [Parallel and Concurrent Programming in Haskell],
disponible en accès libre en ligne.

Pour exécuter les programmes avec plusieurs threads (pour exploiter un
processeur multi-cœur, par exemple), il est nécessaire d’utiliser des
options du RTS. Consultez pour cela la documentation de GHC, en
particulier la section « Running a compiled program »
(`/usr/share/doc/ghc-doc/html/users_guide/runtime-control.html` au M5)
et « Using Concurrent Haskell »
(`/usr/share/doc/ghc-doc/html/users_guide/using-concurrent.html` au M5).

Vous pourrez utiliser notamment les options « `+RTS -lu -N2 -s` ».


[Parallel and Concurrent Programming in Haskell]: http://chimera.labs.oreilly.com/books/1230000000929
