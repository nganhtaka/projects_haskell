Main: $(wildcard *.hs) routes
	ghc -Wall Main.hs

clean:
	rm -f Main *.hi *.dyn_* *.o client_session_key.aes

.PHONY: clean
