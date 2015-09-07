
.PHONY: force clean

build: aex

aex: Parser.hs force
	ghc --make $@

%.hs: %.y
	~/.cabal/bin/happy -gac $<

clean:
	rm aex *.o *.hi Parser.hs

