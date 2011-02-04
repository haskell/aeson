ghc := ghc
ghcflags := -O

binaries := AesonParse EncodeFile JsonParse

all: json-data/example.json $(binaries) $(binaries:%=%_p)

%_p: %.hs
	$(ghc) $(ghcflags) -prof -auto-all --make -o $@ $^

%: %.hs
	$(ghc) $(ghcflags) --make -o $@ $^

json-data/example.json: json-data.tar.bz2
	bzip2 -dc < $^ | tar -m -xf -

clean:
	-rm -f *.o *.hi $(binaries) $(binaries:%=%_p)
