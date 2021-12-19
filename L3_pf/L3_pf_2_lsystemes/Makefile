EXEC = TortueGraph TortueVolante
.PHONY : tortue all

all : tortue tortueVolante

tortue: TortueGraph.hs
	ghc -Wall TortueGraph.hs

tortueVolante: TortueVolante.hs
	ghc -Wall TortueVolante.hs

clean :
	rm -rf *.o
	rm -rf *.hi
	rm -rf $(EXEC)
