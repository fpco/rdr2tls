default: image

cabal.sandbox.config:
	@cabal sandbox init

.cabal-sandbox/bin/bouncy: | cabal.sandbox.config
	@cabal install --ghc-options='-threaded -rtsopts'
	@upx .cabal-sandbox/bin/bouncy

image: | .cabal-sandbox/bin/bouncy
	@docker build -t bouncy .

clean:
	@cabal clean

.PHONY: default image clean
