ghc := ghc

all: qc

qc: Properties.hs
	$(ghc) -o --make $@ $<

clean:
	-rm -f qc *.o *.hi
