.PHONY: all build run dist install clean doc

all: build

build: dist/setup-config
	cabal build

run: build
	./dist/build/fix-id/fix-id

dist: test
	cabal sdist

install: build
	cabal install

clean:
	cabal clean

dist/setup-config: fix-id.cabal
	cabal configure

doc: build
	cabal haddock
