EXEC = dragon dragon2
.PHONY : dragon dragon2

all : dragon dragon2

dragon: dragon.hs
	ghc -Wall dragon.hs

dragon2: dragon2.hs
	ghc -Wall dragon2.hs

clean :
	rm -rf *.o
	rm -rf *.hi
	rm -rf $(EXEC)