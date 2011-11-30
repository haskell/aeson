ghc := ghc
ghcflags := -O

binaries := AesonParse AesonEncode JsonParse AesonCompareAutoInstances

all: $(binaries) $(binaries:%=%_p)

%_p: %.hs
	$(ghc) $(ghcflags) -prof -auto-all -rtsopts --make -o $@ $^

%: %.hs
	$(ghc) $(ghcflags) --make -rtsopts -o $@ $^

clean:
	-rm -f *.o *.hi $(binaries) $(binaries:%=%_p)
