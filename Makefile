
.PHONY: force clean

build: aex

aex: Lexer.hs Parser.hs force
	ghc --make -fno-warn-tabs $@

%.hs: %.x
	~/.cabal/bin/alex -g $<

%.hs: %.y
	~/.cabal/bin/happy -gac $<

clean:
	rm -f aex *.o *.hi Lexer.hs Parser.hs

