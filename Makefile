default: image

cabal.sandbox.config:
	@cabal sandbox init

.cabal-sandbox/bin/rdr2tls: | cabal.sandbox.config
	@cabal install -v2 -j --ghc-options='-fllvm -threaded -rtsopts'
	@upx .cabal-sandbox/bin/rdr2tls

image: | .cabal-sandbox/bin/rdr2tls
	@docker build -t rdr2tls .

clean:
	@cabal clean

.PHONY: default image clean
