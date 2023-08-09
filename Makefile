HC = ghc
#HFLAGS = -O2

09: 09.o
	$(HC) $(HFLAGS) -o 09/$@ 09/$< || make -C 09 -f ../Makefile tidy
	make -C 09 -f ../Makefile tidy

09.o: 09/exercises.hs
	$(HC) -c $< -package gloss -o 09/$@

.PHONY: tidy

tidy: 
	rm -rf *.hi *.o
