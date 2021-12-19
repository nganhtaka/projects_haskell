ALL := somme.o
ALL += Somme.class
ALL += mystere.o
ALL += exemples.o

all: ${ALL}

%.class: %.java
	javac -Xlint:all $<

%.o: %.c
	gcc -Wall -Wextra -c $<

%.o: %.hs
	ghc -Wall $<

clean:
	rm -f ${ALL} *.o *.hi

.PHONY: all clean
