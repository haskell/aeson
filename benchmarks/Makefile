ghc := ghc
ghcflags := -O

binaries := AesonParse EncodeFile JsonParse

all: json-data/example.json $(binaries)

AesonParse: AesonParse.hs
	$(ghc) $(ghcflags) --make -o $@ $^

EncodeFile: EncodeFile.hs
	$(ghc) $(ghcflags) --make -o $@ $^

JsonParse: JsonParse.hs
	$(ghc) $(ghcflags) --make -o $@ $^

json-data/example.json: json-data.tar.bz2
	bzip2 -dc < $^ | tar -m -xf -

clean:
	-rm -f *.o *.hi $(binaries)
