# TODO: Should I use Cabal for this?

.PHONY: build test clean force

test: build
	time ./aex

build: aex

aex: Lexer.hs Parser.hs force
	ghc --make -O2 -fno-warn-tabs $@

%.hs: %.x
	~/.cabal/bin/alex -g $<

%.hs: %.y
	~/.cabal/bin/happy -gac $<

clean:
	rm -f aex *.o *.hi Lexer.hs Parser.hs

