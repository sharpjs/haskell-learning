
.PHONY: force clean

build: aex

aex: aex.hs force
	gcc --make $@

%.hs: %.y
	~/.cabal/bin/happy -gac $<

clean:
	rm aex *.o *.hi aex.hs

