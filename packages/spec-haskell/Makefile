CABAL ?= cabal
TEST_TARGET ?= test-suite

build:
	$(CABAL) v2-build

clean:
	$(CABAL) v1-clean
	$(CABAL) v2-clean

test-suite:
	$(CABAL) v2-test

test-demo:
	$(CABAL) v2-exec -- superfluid-protocol-validator --demo

test: $(TEST_TARGET)

dev:
	npx nodemon -e hs,cabal -i dist -i dist-newstyle -x "make build test || exit 1"

loc:
	@echo '**** Core Source Files ****'
	find src simple validator -name '*.hs' | xargs wc -l 
	@echo '**** Non-Core Source Files ****'
	find simple validator -name '*.hs' | xargs wc -l 
	@echo '**** Test Files ****'
	find test -name '*.hs' | xargs wc -l

.PHONY: build clean test-suite test-demo test dev loc
