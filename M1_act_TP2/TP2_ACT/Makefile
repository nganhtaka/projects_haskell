EXEC = HexapawnV1 HexapawnV2 HexapawnV3
.PHONY : all

all : hexv1 hexv2 hexv3

hexv1: HexapawnV1.hs
	ghc -Wall HexapawnV1.hs

hexv2: HexapawnV2.hs
	ghc -Wall HexapawnV2.hs

hexv3: HexapawnV3.hs
	ghc -Wall HexapawnV3.hs

clean :
	rm -rf *.o
	rm -rf *.hi
	rm -rf $(EXEC)
