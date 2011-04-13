ghc := ghc

all: qc

qc: Properties.hs
	$(ghc) -Wall -o --make $@ $<

clean:
	-rm -f qc *.o *.hi
