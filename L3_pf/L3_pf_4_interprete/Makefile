EXEC = Main Interprete
.PHONY : main interprete all

all : main

main: Main.hs
	ghc --make Main

interprete: Interprete.hs
	ghc -Wall -Werror -dynamic -fdefer-typed-holes Interprete.hs

clean :
	rm -rf *.o
	rm -rf *.hi
	rm -rf $(EXEC)
