ghc := ghc

all: qc

qc: Properties.hs
	$(ghc) -Wall -o -threaded --make $@ $<

clean:
	-rm -f qc *.o *.hi
