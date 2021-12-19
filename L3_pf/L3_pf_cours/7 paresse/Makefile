tout: fibo test-fibo test-io1 test-io2

fibo: Paresse.hs
	ghc -main-is Paresse.mainFibo Paresse.hs -o fibo

test-fibo:
	./fibo 100000 +RTS -s

test-io1:
	ghc -e Paresse.mainIO Paresse.hs

test-io2:
	ghc -e Paresse.mainIO2 Paresse.hs

.PHONY: tout test-fibo test-io1 test-io2
