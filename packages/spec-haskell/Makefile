CABAL ?= cabal
TEST_TARGET ?= test-suite

build:
	$(CABAL) v2-build

test-suite:
	$(CABAL) v2-test

test-demo:
	$(CABAL) v2-exec -- superfluid-protocol-validator --demo

test: $(TEST_TARGET)

clean:
	$(CABAL) v1-clean
	$(CABAL) v2-clean

dev:
	npx nodemon -e hs,cabal -i dist -i dist-newstyle -x "make build test || exit 1"

.PHONY: build clean test-suite test-demo test dev
