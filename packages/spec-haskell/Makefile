CABAL=cabal

build:
	$(CABAL) v2-build

test:
	$(CABAL) v2-exec superfluid-protocol-validator
	$(CABAL) v2-test

clean:
	$(CABAL) v1-clean
	$(CABAL) v2-clean

dev:
	npx nodemon -e hs,cabal -i dist-newstyle -x 'make build test || exit 1'

.PHONY: build clean test dev
