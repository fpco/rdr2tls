default: docker-integer-gmp

cabal.sandbox.config:
	@cabal sandbox init

.cabal-sandbox/bin/rdr2tls: | cabal.sandbox.config
	@cabal install -v2 -j --ghc-options='-fllvm -threaded -rtsopts'
	@upx .cabal-sandbox/bin/rdr2tls

image: | .cabal-sandbox/bin/rdr2tls
	@docker build -t rdr2tls .

docker-integer-gmp: | .cabal-sandbox/bin/rdr2tls
	@ln -sf Dockerfile-gmp Dockerfile
	@docker build -t rdr2tls:integer-gmp .

docker-integer-simple: | .cabal-sandbox/bin/rdr2tls
	@ln -sf Dockerfile-simple Dockerfile
	@docker build -t rdr2tls:integer-simple .

clean:
	@cabal clean
	@rm Dockerfile

.PHONY: default docker-integer-gmp docker-integer-simple clean
