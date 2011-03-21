ghc := ghc
ghcflags := -O

binaries := AesonParse AesonEncode JsonParse

all: $(binaries) $(binaries:%=%_p)

%_p: %.hs
	$(ghc) $(ghcflags) -prof -auto-all --make -o $@ $^

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ $^

clean:
	-rm -f *.o *.hi $(binaries) $(binaries:%=%_p)
