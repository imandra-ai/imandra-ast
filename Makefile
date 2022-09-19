
DUNE_OPTS ?= --display=progress --profile=release

all: build test

build:
	@dune build $(DUNE_OPTS) @install

test:
	@dune build $(DUNE_OPTS) @runtest

clean:
	@dune clean

format:
	dune build @fmt --display=quiet --auto-promote

WATCH?= @check
watch:
	dune build $(WATCH) -j 3 $(DUNE_OPTS) -w
